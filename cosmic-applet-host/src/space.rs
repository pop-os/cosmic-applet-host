// SPDX-License-Identifier: MPL-2.0-only

use std::{
    cell::{Cell, RefCell},
    default::Default,
    ffi::OsString,
    fs, mem,
    os::unix::{net::UnixStream, prelude::AsRawFd},
    process::Child,
    rc::Rc,
    time::Instant,
};
use std::thread::spawn;

use anyhow::{anyhow, bail, Context};
use freedesktop_desktop_entry::{self, DesktopEntry, Iter};
use itertools::{Itertools, izip};
use libc::{c_int, qsort};
use sctk::{
    environment::Environment,
    output::OutputInfo,
    reexports::{
        client::{self, Attached, Main},
        client::protocol::{wl_output as c_wl_output, wl_surface as c_wl_surface},
        protocols::{
            wlr::unstable::layer_shell::v1::client::{
                zwlr_layer_shell_v1::{self, Layer},
                zwlr_layer_surface_v1,
            },
            xdg_shell::client::{
                xdg_popup,
                xdg_positioner::{Anchor, Gravity, XdgPositioner},
                xdg_surface::{self, XdgSurface},
                xdg_wm_base::{self, XdgWmBase},
            },
        },
    },
    shm::AutoMemPool,
};
use slog::{info, Logger, trace};
use smithay::{
    backend::{
        egl::{
            self,
            context::{EGLContext, GlAttributes},
            display::EGLDisplay,
            ffi::{
                self,
                egl::{GetConfigAttrib, SwapInterval},
            },
            surface::EGLSurface,
        },
        renderer::{
            Bind, Frame, gles2::Gles2Renderer, ImportDma, ImportEgl, Renderer,
            Unbind, utils::draw_surface_tree,
        },
    },
    desktop::{
        draw_popups, draw_window,
        Kind,
        PopupKind,
        PopupManager, space::{RenderError, SurfaceTree}, Space, utils::{bbox_from_surface_tree, damage_from_surface_tree, send_frames_surface_tree}, Window, WindowSurfaceType,
    },
    nix::{fcntl, libc},
    reexports::{
        wayland_protocols::xdg::shell::server::xdg_toplevel,
        wayland_server::{
            self, Client, Display as s_Display, DisplayHandle,
            protocol::wl_surface::WlSurface as s_WlSurface, Resource,
        },
    },
    utils::{Logical, Physical, Point, Rectangle, Size},
    wayland::{
        SERIAL_COUNTER,
        shell::xdg::{PopupSurface, PositionerState},
    },
};
use smithay::backend::renderer::buffer_dimensions;
use smithay::desktop::space::RenderZindex;
use smithay::reexports::wayland_server::backend::ClientId;
use wayland_egl::WlEglSurface;
use xdg_shell_wrapper::{
    client_state::{Env, Focus},
    config::WrapperConfig,
    space::{ClientEglSurface, Popup, PopupState, SpaceEvent, Visibility, WrapperSpace},
    util::{exec_child, get_client_sock},
};
use zbus::export::futures_util::{StreamExt, TryFutureExt};

use cosmic_applet_host_config::{AppletConfig, AppletHostConfig};

#[derive(Debug)]
pub struct ActiveApplet {
    pub(crate) s_wl_surface: s_WlSurface,
    pub(crate) name: String,
    pub(crate) layer_surface: Main<zwlr_layer_surface_v1::ZwlrLayerSurfaceV1>,
    pub(crate) egl_surface: Option<Rc<EGLSurface>>,
    pub(crate) layer_shell_wl_surface: Attached<c_wl_surface::WlSurface>,
    pub(crate) w_accumulated_damage: Vec<Vec<Rectangle<i32, Physical>>>,
    pub(crate) popups: Vec<Popup>,
    // tracking for drawing
    pub(crate) pending_dimensions: Option<Size<i32, Logical>>,
    pub(crate) full_clear: bool,
    pub(crate) should_render: bool,
    pub(crate) next_render_event: Rc<Cell<Option<SpaceEvent>>>,
    pub(crate) dimensions: Size<i32, Logical>,
    pub(crate) config: AppletConfig,
}

impl Drop for ActiveApplet {
    fn drop(&mut self) {
        self.layer_surface.destroy();
        self.layer_shell_wl_surface.destroy();
    }
}

impl ActiveApplet {
    // Handles any events that have occurred since the last call, redrawing if needed. Returns true if the surface is alive.
    pub(crate) fn handle_events(
        &mut self,
        log: Logger,
        c_display: &client::Display,
        renderer: &Gles2Renderer,
        egl_display: &EGLDisplay,
    ) -> bool {
        dbg!(&self.next_render_event);
        match self.next_render_event.take() {
            Some(SpaceEvent::Quit) => {
                return false;
            }
            Some(SpaceEvent::Configure {
                first,
                width,
                height,
                serial: _serial,
            }) => {
                self.full_clear = true;
                if first {
                    let client_egl_surface = ClientEglSurface {
                        wl_egl_surface: WlEglSurface::new(
                            &self.layer_shell_wl_surface,
                            width,
                            height,
                        ),
                        display: c_display.clone(),
                    };

                    let egl_surface = Rc::new(
                        EGLSurface::new(
                            &egl_display,
                            renderer
                                .egl_context()
                                .pixel_format()
                                .expect("Failed to get pixel format from EGL context "),
                            renderer.egl_context().config_id(),
                            client_egl_surface,
                            log.clone(),
                        )
                            .expect("Failed to initialize EGL Surface"),
                    );

                    self.egl_surface.replace(egl_surface);
                } else {
                    self.egl_surface
                        .as_ref()
                        .unwrap()
                        .resize(width as i32, height as i32, 0, 0);
                }
                self.layer_shell_wl_surface.commit();
            }
            Some(SpaceEvent::WaitConfigure {
                first,
                width,
                height,
            }) => {
                self.next_render_event
                    .replace(Some(SpaceEvent::WaitConfigure {
                        first,
                        width,
                        height,
                    }));
            }
            None => {
                if let Some(d) = self.pending_dimensions.take() {
                    println!("HANDLING PENDING DIMENSTIONS");
                    self.layer_surface
                        .set_size(d.w.try_into().unwrap(), d.h.try_into().unwrap());
                    self.layer_shell_wl_surface.commit();
                    self.next_render_event
                        .replace(Some(SpaceEvent::WaitConfigure {
                            first: false,
                            width: d.w,
                            height: d.h,
                        }));
                } else if self.egl_surface.as_ref().unwrap().get_size()
                    != Some(self.dimensions.to_physical(1))
                {
                    self.full_clear = true;
                } else {
                    self.should_render = true;
                }
            }
        }
        return true;
    }
}

/// space for the cosmic panel
#[derive(Debug)]
pub struct AppletHostSpace {
    /// config for the panel space
    pub config: AppletHostConfig,
    /// logger for the panel space
    pub log: Option<Logger>,
    pub(crate) space: Space,
    pub(crate) popup_manager: PopupManager,
    pub(crate) clients: Vec<(Client, String)>,
    pub(crate) children: Vec<Child>,
    pub(crate) last_dirty: Option<Instant>,
    /// focused surface so it can be changed when a window is removed
    focused_surface: Rc<RefCell<Option<s_WlSurface>>>,
    /// visibility state of the panel / panel
    pub(crate) pool: Option<AutoMemPool>,
    pub(crate) layer_shell: Option<Attached<zwlr_layer_shell_v1::ZwlrLayerShellV1>>,
    pub(crate) output: Option<(c_wl_output::WlOutput, OutputInfo)>,
    pub(crate) c_display: Option<client::Display>,
    pub(crate) egl_display: Option<EGLDisplay>,
    pub(crate) renderer: Option<Gles2Renderer>,
    pub(crate) egl_surface: Option<Rc<EGLSurface>>,
    pub(crate) layer_shell_wl_surface: Option<Attached<c_wl_surface::WlSurface>>,
    pub(crate) layer_surface: Option<Main<zwlr_layer_surface_v1::ZwlrLayerSurfaceV1>>,
    pub(crate) next_space_event: Rc<Cell<Option<SpaceEvent>>>,
    active_applets: Vec<ActiveApplet>,
    start: Instant,
    first_draw: bool,
}

impl AppletHostSpace {
    /// create a new space for the cosmic panel
    pub fn new(config: AppletHostConfig, log: Logger) -> Self {
        Self {
            config,
            space: Space::new(log.clone()),
            popup_manager: PopupManager::new(log.clone()),
            log: Some(log),
            clients: Default::default(),
            children: Default::default(),
            last_dirty: Default::default(),
            focused_surface: Default::default(),
            pool: Default::default(),
            layer_shell: Default::default(),
            output: Default::default(),
            c_display: Default::default(),
            egl_display: Default::default(),
            renderer: Default::default(),
            egl_surface: Default::default(),
            layer_shell_wl_surface: Default::default(),
            active_applets: Default::default(),
            start: Instant::now(),
            next_space_event: Rc::new(Cell::new(None)),
            layer_surface: None,
            first_draw: true,
        }
    }

    fn close_popups(&mut self) {
        for w in &mut self.space.windows() {
            for (PopupKind::Xdg(p), _) in
            PopupManager::popups_for_surface(w.toplevel().wl_surface())
            {
                p.send_popup_done();
                self.popup_manager.commit(p.wl_surface());
            }
        }
    }

    fn constrain_dim(&self, size: Size<i32, Logical>, applet: &str) -> Size<i32, Logical> {
        let mut w = size.w.try_into().unwrap();
        let mut h = size.h.try_into().unwrap();
        w = 1.max(w);
        h = 1.max(h);
        let d = self.config.dimensions(applet);
        if let Some((Some(w_range), _)) = d.as_ref() {
            if w < w_range.start {
                w = w_range.start;
            } else if w > w_range.end {
                w = w_range.end;
            }
        }
        if let Some((_, Some(h_range))) = d.as_ref() {
            if h < h_range.start {
                h = h_range.start;
            } else if h > h_range.end {
                h = h_range.end;
            }
        }
        (w.try_into().unwrap(), h.try_into().unwrap()).into()
    }

    fn render(&mut self, time: u32) -> Result<(), RenderError<Gles2Renderer>> {
        let clear_color = [0.0, 0.0, 0.0, 0.0];
        let renderer = if let Some(r) = self.renderer.as_mut() {
            r
        } else {
            return Ok(());
        };

        let log_clone = self.log.clone().unwrap();

        for mut active_applet in &mut self.active_applets {
            dbg!(&active_applet);

            let w = if let Some(w) = self
                .space
                .window_for_surface(&active_applet.s_wl_surface, WindowSurfaceType::TOPLEVEL)
            {
                w
            } else {
                continue;
            };

            let o = if let Some(o) = self.space.outputs_for_window(w).pop() {
                o
            } else {
                continue;
            };

            let output_size = o.current_mode().ok_or(RenderError::OutputNoMode)?.size;
            // TODO handle fractional scaling?
            // let output_scale = o.current_scale().fractional_scale();
            // We explicitly use ceil for the output geometry size to make sure the damage
            // spans at least the output size. Round and floor would result in parts not drawn as the
            // frame size could be bigger than the maximum the output_geo would define.
            let output_geo =
                Rectangle::from_loc_and_size(o.current_location(), output_size.to_logical(1));

            if active_applet.should_render {
                active_applet.should_render = false;
                let cur_damage = if active_applet.full_clear {
                    vec![
                        (Rectangle::from_loc_and_size(
                            (0, 0),
                            active_applet.dimensions.to_physical(1),
                        )),
                    ]
                } else {
                    w.accumulated_damage(
                        w.geometry().loc.to_f64().to_physical(1.0),
                        1.0,
                        Some((&self.space, &o)),
                    )
                };

                if let Some(mut damage) = Self::damage_for_buffer(
                    cur_damage,
                    &mut active_applet.w_accumulated_damage,
                    active_applet.egl_surface.as_ref().unwrap(),
                ) {
                    if damage.is_empty() {
                        damage.push(Rectangle::from_loc_and_size(
                            (0, 0),
                            active_applet.dimensions.to_physical(1),
                        ));
                    }
                    let w_loc = self.space.window_location(&w).unwrap_or_else(|| (0, 0).into());
                    let _ = renderer.unbind();
                    renderer
                        .bind(active_applet.egl_surface.as_ref().unwrap().clone())
                        .expect("Failed to bind surface to GL");
                    let _ = renderer.render(
                        active_applet.dimensions.to_physical(1),
                        smithay::utils::Transform::Flipped180,
                        |renderer: &mut Gles2Renderer, frame| {
                            frame
                                .clear(clear_color, damage.iter().cloned().collect_vec().as_slice())
                                .expect("Failed to clear frame.");
                            for w in self.space.windows() {
                                if damage.len() == 0 {
                                    continue;
                                }

                                let _ = draw_window(
                                    renderer,
                                    frame,
                                    w,
                                    1.0,
                                    w_loc.to_physical(1).to_f64(),
                                    &damage,
                                    &log_clone,
                                );
                            }
                        },
                    );
                    active_applet
                        .egl_surface
                        .as_ref()
                        .unwrap()
                        .swap_buffers(Some(&mut damage))
                        .expect("Failed to swap buffers.");
                }
            }
            // Popup rendering
            for p in active_applet.popups.iter_mut().filter(|p| {
                p.dirty
                    && match p.popup_state.get() {
                        None => true,
                        _ => false,
                    }
            }) {
                let _ = renderer.unbind();
                renderer
                    .bind(p.egl_surface.clone())
                    .expect("Failed to bind surface to GL");
                let p_bbox = bbox_from_surface_tree(p.s_surface.wl_surface(), (0, 0));
                let cur_damage = if active_applet.full_clear {
                    vec![p_bbox.to_physical(1)]
                } else {
                    damage_from_surface_tree(
                        p.s_surface.wl_surface(),
                        p_bbox.loc.to_f64().to_physical(1.0),
                        1.0,
                        Some((&self.space, &o)),
                    )
                };

                if let Some(mut damage) =
                    Self::damage_for_buffer(cur_damage, &mut p.accumulated_damage, &p.egl_surface)
                {
                    if damage.is_empty() {
                        damage.push(p_bbox.to_physical(1));
                    }

                    let _ = renderer.render(
                        p_bbox.size.to_physical(1),
                        smithay::utils::Transform::Flipped180,
                        |renderer: &mut Gles2Renderer, frame| {
                            frame
                                .clear(clear_color, damage.iter().cloned().collect_vec().as_slice())
                                .expect("Failed to clear frame.");

                            let _ = draw_surface_tree(
                                renderer,
                                frame,
                                p.s_surface.wl_surface(),
                                1.0,
                                p_bbox.loc.to_f64().to_physical(1.0),
                                &damage,
                                &log_clone,
                            );
                        },
                    );
                    p.egl_surface
                        .swap_buffers(Some(&mut damage))
                        .expect("Failed to swap buffers.");
                    p.dirty = false;
                }
            }
            active_applet.full_clear = false;
        }

        self.space.send_frames(time);
        Ok(())
    }

    fn damage_for_buffer(
        cur_damage: Vec<Rectangle<i32, Physical>>,
        acc_damage: &mut Vec<Vec<Rectangle<i32, Physical>>>,
        egl_surface: &Rc<EGLSurface>,
    ) -> Option<Vec<Rectangle<i32, Physical>>> {
        let mut age: usize = egl_surface
            .buffer_age()
            .unwrap_or_default()
            .try_into()
            .unwrap_or_default();
        let dmg_counts = acc_damage.len();

        let ret = if age == 0 || age > dmg_counts {
            Some(Vec::new())
        } else {
            if !cur_damage.is_empty() {
                acc_damage.push(cur_damage);
                age += 1;
            }
            let mut d = acc_damage.clone();
            d.reverse();
            let d = d[..age + 1]
                .into_iter()
                .map(|v| v.into_iter().cloned())
                .flatten()
                .collect_vec();
            if d.is_empty() {
                // pop front because it won't actually be used
                None
            } else {
                Some(d)
            }
        };

        if acc_damage.len() > 4 {
            acc_damage.drain(..acc_damage.len() - 4);
        }

        // dbg!(age, dmg_counts, &acc_damage, &ret);
        ret
    }

    fn z_index(&self, applet: &str) -> Option<RenderZindex> {
        match self.config.layer(applet) {
            Some(Layer::Background) => Some(RenderZindex::Background),
            Some(Layer::Bottom) => Some(RenderZindex::Bottom),
            Some(Layer::Top) => Some(RenderZindex::Top),
            Some(Layer::Overlay) => Some(RenderZindex::Overlay),
            _ => None,
        }
    }

    fn applet_config_for_window(&self, w: &Window) -> Option<&AppletConfig> {
        let client_id = w.toplevel().wl_surface().client_id();
        self.clients
            .iter()
            .find_map(|c| {
                if client_id == Some(c.0.id()) {
                    Some(c.1.clone())
                } else {
                    None
                }
            })
            .and_then(|name| self.config.applet(&name))
    }

    pub fn toggle_applet(
        &mut self,
        applet_name: &str,
        c_surface: Attached<c_wl_surface::WlSurface>,
    ) -> anyhow::Result<()> {
        // cleanup
        if let Some(i) = self
            .active_applets
            .iter()
            .position(|a| a.name == applet_name.to_string())
        {
            let mut active_applet = self.active_applets.swap_remove(i);
            return Ok(());
        }

        let w = self
            .clients
            .iter()
            .find(|c| c.1 == applet_name)
            .and_then(|active| {
                self.space.windows().find_map(|w| {
                    let c_id = active.0.id();
                    if Some(&c_id) == w.toplevel().wl_surface().client_id().as_ref() {
                        Some(w)
                    } else {
                        None
                    }
                })
            }).cloned();
        if let Some(w) = w {
            self.space.raise_window(&w, true);
            let s_wl_surface = w.toplevel().wl_surface();
            let dimensions = w.bbox().size;
            let layer_surface = self.layer_shell.as_ref().unwrap().get_layer_surface(
                &c_surface,
                self.output.as_ref().map(|o| &o.0),
                self.config.layer(applet_name).unwrap(),
                "".to_owned(),
            );

            layer_surface.set_anchor(self.config.anchor(applet_name).unwrap().into());
            layer_surface.set_keyboard_interactivity(
                self.config.keyboard_interactivity(applet_name).unwrap(),
            );
            layer_surface.set_size(
                dimensions.w.try_into().unwrap(),
                dimensions.h.try_into().unwrap(),
            );

            // Commit so that the server will send a configure event
            c_surface.commit();

            let next_render_event = Rc::new(Cell::new(Some(SpaceEvent::WaitConfigure {
                first: true,
                width: dimensions.w,
                height: dimensions.h,
            })));

            let next_render_event_handle = next_render_event.clone();
            let logger = self.log.clone().unwrap();
            layer_surface.quick_assign(move |layer_surface, event, _| {
                match (event, next_render_event_handle.get()) {
                    (zwlr_layer_surface_v1::Event::Closed, _) => {
                        info!(logger, "Received close event. closing.");
                        next_render_event_handle.set(Some(SpaceEvent::Quit));
                    }
                    (
                        zwlr_layer_surface_v1::Event::Configure {
                            serial,
                            width,
                            height,
                        },
                        next,
                    ) if next != Some(SpaceEvent::Quit) => {
                        trace!(
                            logger,
                            "received configure event {:?} {:?} {:?}",
                            serial,
                            width,
                            height
                        );
                        layer_surface.ack_configure(serial);
                        let first = match next {
                            Some(SpaceEvent::Configure { first, .. }) => first,
                            Some(SpaceEvent::WaitConfigure { first, .. }) => first,
                            _ => false,
                        };
                        next_render_event_handle.set(Some(SpaceEvent::Configure {
                            first,
                            width: if width == 0 {
                                dimensions.w
                            } else {
                                width.try_into().unwrap()
                            },
                            height: if height == 0 {
                                dimensions.h
                            } else {
                                height.try_into().unwrap()
                            },
                            serial: serial.try_into().unwrap(),
                        }));
                    }
                    (_, _) => {}
                }
            });

            self.active_applets.push(ActiveApplet {
                name: applet_name.to_string(),
                layer_surface,
                egl_surface: None,
                layer_shell_wl_surface: c_surface,
                w_accumulated_damage: vec![],
                s_wl_surface: s_wl_surface.clone(),
                popups: Default::default(),
                pending_dimensions: None,
                full_clear: true,
                should_render: false,
                next_render_event,
                dimensions,
                config: self.config.applet(applet_name).unwrap().clone(),
            });
            return Ok(());
        }
        anyhow::bail!("No client with the requested name!");
    }
}

impl WrapperSpace for AppletHostSpace {
    type Config = AppletHostConfig;

    fn handle_events(&mut self, dh: &DisplayHandle, time: u32, _: &Focus) -> Instant {
        if self
            .children
            .iter_mut()
            .map(|c| c.try_wait())
            .all(|r| matches!(r, Ok(Some(_))))
        {
            info!(
                self.log.as_ref().unwrap().clone(),
                "Child processes exited. Now exiting..."
            );
            std::process::exit(0);
        }
        let windows = self.space.windows().cloned().collect_vec();

        self.active_applets.retain_mut(|a| {
            a.popups
                .retain_mut(|p: &mut Popup| p.handle_events(&mut self.popup_manager));
            a.handle_events(
                self.log.as_ref().unwrap().clone(),
                self.c_display.as_ref().unwrap(),
                self.renderer.as_ref().unwrap(),
                self.egl_display.as_ref().unwrap(),
            )
        });
        let _ = self.render(time);

        self.last_dirty.unwrap_or_else(|| self.start)
    }

    fn popups(&self) -> Vec<&Popup> {
        self.active_applets
            .iter()
            .map(|a| &a.popups)
            .flatten()
            .collect_vec()
    }

    fn handle_button(&mut self, c_focused_surface: &c_wl_surface::WlSurface) {
        let matches_any_client = self
            .active_applets
            .iter()
            .any(|a| *a.layer_shell_wl_surface == *c_focused_surface);
        if self.focused_surface.borrow().is_none() && matches_any_client {
            self.close_popups()
        }
    }

    fn add_window(&mut self, w: Window) {
        // TODO find active applet with matching client and full clear
        // self.full_clear = true;
        let wl_surface = w.toplevel().wl_surface().clone();

        let name = if let Some(applet_config) = self.applet_config_for_window(&w) {
            applet_config.name.clone()
        } else {
            return;
        };
        self.space
            .map_window(&w, (0, 0), self.z_index(&name).map(|z| z as u8), false);
        // self.space.raise_window(&w, false);
        for w in self.space.windows() {
            w.configure();
        }
    }

    fn add_popup(
        &mut self,
        env: &Environment<Env>,
        xdg_wm_base: &Attached<XdgWmBase>,
        s_surface: PopupSurface,
        positioner: Main<XdgPositioner>,
        PositionerState {
            rect_size,
            anchor_rect,
            anchor_edges,
            gravity,
            constraint_adjustment,
            offset,
            reactive,
            parent_size,
            parent_configure,
        }: PositionerState,
    ) {
        self.close_popups();

        let s = if let Some(s) = self.space.windows().find(|w| match w.toplevel() {
            Kind::Xdg(wl_s) => Some(wl_s.wl_surface()) == s_surface.get_parent_surface().as_ref(),
        }) {
            s
        } else {
            return;
        };

        let active_applet = if let Some(a) = self
            .active_applets
            .iter_mut()
            .find(|a| Some(&a.s_wl_surface) == s_surface.get_parent_surface().as_ref())
        {
            a
        } else {
            return;
        };

        let c_wl_surface = env.create_surface().detach();
        let c_xdg_surface = xdg_wm_base.get_xdg_surface(&c_wl_surface);

        let wl_surface = s_surface.wl_surface().clone();
        let s_popup_surface = s_surface.clone();
        self.popup_manager
            .track_popup(PopupKind::Xdg(s_surface.clone()))
            .unwrap();
        self.popup_manager.commit(&wl_surface);

        // dbg!(s.bbox().loc);
        positioner.set_size(rect_size.w, rect_size.h);
        positioner.set_anchor_rect(
            anchor_rect.loc.x + s.bbox().loc.x,
            anchor_rect.loc.y + s.bbox().loc.y,
            anchor_rect.size.w,
            anchor_rect.size.h,
        );
        positioner.set_anchor(Anchor::from_raw(anchor_edges as u32).unwrap_or(Anchor::None));
        positioner.set_gravity(Gravity::from_raw(gravity as u32).unwrap_or(Gravity::None));

        positioner.set_constraint_adjustment(u32::from(constraint_adjustment));
        positioner.set_offset(offset.x, offset.y);
        if positioner.as_ref().version() >= 3 {
            if reactive {
                positioner.set_reactive();
            }
            if let Some(parent_size) = parent_size {
                positioner.set_parent_size(parent_size.w, parent_size.h);
            }
        }
        let c_popup = c_xdg_surface.get_popup(None, &positioner);
        active_applet.layer_surface.get_popup(&c_popup);

        //must be done after role is assigned as popup
        c_wl_surface.commit();

        let cur_popup_state = Rc::new(Cell::new(Some(PopupState::WaitConfigure)));
        c_xdg_surface.quick_assign(move |c_xdg_surface, e, _| {
            if let xdg_surface::Event::Configure { serial, .. } = e {
                c_xdg_surface.ack_configure(serial);
            }
        });

        let popup_state = cur_popup_state.clone();

        c_popup.quick_assign(move |_c_popup, e, _| {
            if let Some(PopupState::Closed) = popup_state.get().as_ref() {
                return;
            }

            match e {
                xdg_popup::Event::Configure {
                    x,
                    y,
                    width,
                    height,
                } => {
                    if popup_state.get() != Some(PopupState::Closed) {
                        let _ = s_popup_surface.send_configure();
                        popup_state.set(Some(PopupState::Configure {
                            x,
                            y,
                            width,
                            height,
                        }));
                    }
                }
                xdg_popup::Event::PopupDone => {
                    popup_state.set(Some(PopupState::Closed));
                }
                xdg_popup::Event::Repositioned { token } => {
                    popup_state.set(Some(PopupState::Repositioned(token)));
                }
                _ => {}
            };
        });
        let client_egl_surface = ClientEglSurface {
            wl_egl_surface: WlEglSurface::new(&c_wl_surface, rect_size.w, rect_size.h),
            display: self.c_display.as_ref().unwrap().clone(),
        };

        let egl_context = self.renderer.as_ref().unwrap().egl_context();
        let egl_surface = Rc::new(
            EGLSurface::new(
                &self.egl_display.as_ref().unwrap(),
                egl_context
                    .pixel_format()
                    .expect("Failed to get pixel format from EGL context "),
                egl_context.config_id(),
                client_egl_surface,
                self.log.clone(),
            )
            .expect("Failed to initialize EGL Surface"),
        );

        active_applet.popups.push(Popup {
            c_popup,
            c_xdg_surface,
            c_wl_surface: c_wl_surface,
            s_surface,
            egl_surface,
            dirty: false,
            popup_state: cur_popup_state,
            position: (0, 0).into(),
            accumulated_damage: Default::default(),
        });
    }

    // TODO fix this
    ///  update active window based on pointer location
    fn update_pointer(&mut self, (x, y): (i32, i32)) {
        // set new focused
        if let Some((_, s, _)) = self
            .space
            .surface_under((x as f64, y as f64), WindowSurfaceType::ALL)
        {
            self.focused_surface.borrow_mut().replace(s);
            return;
        }
        self.focused_surface.borrow_mut().take();
    }

    fn reposition_popup(
        &mut self,
        s_popup: PopupSurface,
        _: Main<XdgPositioner>,
        _: PositionerState,
        token: u32,
    ) -> anyhow::Result<()> {
        s_popup.send_repositioned(token);
        s_popup.send_configure()?;
        self.popup_manager.commit(s_popup.wl_surface());

        Ok(())
    }

    // TODO replace this method
    fn next_space_event(&self) -> Rc<Cell<Option<SpaceEvent>>> {
        Rc::new(Cell::new(None))
    }

    fn config(&self) -> Self::Config {
        self.config.clone()
    }

    fn spawn_clients(
        &mut self,
        display: &mut DisplayHandle,
    ) -> Result<Vec<UnixStream>, anyhow::Error> {
        if self.children.is_empty() {
            let (clients, sockets): (Vec<_>, Vec<_>) = self
                .config
                .applets
                .iter()
                .map(|a| {
                    let (c, s) = get_client_sock(display);
                    ((c, a.name.clone()), s)
                })
                .unzip();
            self.clients = clients;
            self.children = Iter::new(freedesktop_desktop_entry::default_paths())
                .filter_map(|path| {
                    self.clients
                        .iter()
                        .zip(&sockets)
                        .find(|((_, app_file_name), _)| {
                            Some(OsString::from(&app_file_name).as_os_str()) == path.file_stem()
                        })
                        .and_then(|(_, client_socket)| {
                            fs::read_to_string(&path).ok().and_then(|bytes| {
                                if let Ok(entry) = DesktopEntry::decode(&path, &bytes) {
                                    if let Some(exec) = entry.exec() {
                                        let requests_host_wayland_display =
                                            entry.desktop_entry("HostWaylandDisplay").is_some();
                                        return Some(exec_child(
                                            exec,
                                            Some(self.config.name()),
                                            self.log.as_ref().unwrap().clone(),
                                            client_socket.as_raw_fd(),
                                            requests_host_wayland_display,
                                        ));
                                    }
                                }
                                None
                            })
                        })
                })
                .collect_vec();

            Ok(sockets)
        } else {
            bail!("Clients have already been spawned!");
        }
    }

    fn add_output(
        &mut self,
        output: Option<&c_wl_output::WlOutput>,
        output_info: Option<&OutputInfo>,
        pool: AutoMemPool,
        c_display: client::Display,
        layer_shell: Attached<zwlr_layer_shell_v1::ZwlrLayerShellV1>,
        log: Logger,
        c_surface: Attached<c_wl_surface::WlSurface>,
        focused_surface: Rc<RefCell<Option<s_WlSurface>>>,
    ) -> anyhow::Result<()> {
        let dimensions: Size<i32, Physical> = (30, 30).into();
        if self.output.is_some() {
            bail!("output already added!")
        }


        let client_egl_surface = ClientEglSurface {
            wl_egl_surface: WlEglSurface::new(
                &c_surface,
                1,
                1,
            ),
            display: c_display.clone(),
        };
        let egl_display = EGLDisplay::new(&client_egl_surface, log.clone())
            .expect("Failed to initialize EGL display");

        let egl_context = EGLContext::new_with_config(
            &egl_display,
            GlAttributes {
                version: (3, 0),
                profile: None,
                debug: cfg!(debug_assertions),
                vsync: false,
            },
            Default::default(),
            log.clone(),
        )
            .expect("Failed to initialize EGL context");

        let renderer = unsafe {
            Gles2Renderer::new(egl_context, log.clone())
                .expect("Failed to initialize EGL Surface")
        };

        self.renderer.replace(renderer);
        self.egl_display.replace(egl_display);
        self.layer_shell_wl_surface.replace(c_surface);
        self.layer_shell.replace(layer_shell);
        self.focused_surface = focused_surface.clone();
        self.output = output.cloned().zip(output_info.cloned());
        self.pool.replace(pool);
        self.c_display.replace(c_display);

        Ok(())
    }

    fn log(&self) -> Option<Logger> {
        self.log.clone()
    }

    fn destroy(&mut self) {
        for a in &mut self.active_applets {
            a.layer_surface.destroy();
            a.layer_shell_wl_surface.destroy();
        }
    }

    fn space(&mut self) -> &mut Space {
        &mut self.space
    }

    fn popup_manager(&mut self) -> &mut PopupManager {
        &mut self.popup_manager
    }

    fn raise_window(&mut self, w: &Window, activate: bool) {
        self.space.raise_window(w, activate);
    }

    fn dirty_window(&mut self, dh: &DisplayHandle, s: &s_WlSurface) {
        self.space.commit(&s);
        self.space.refresh(&dh);
        self.last_dirty = Some(Instant::now());
        let w = if let Some(w) = self.space.window_for_surface(s, WindowSurfaceType::ALL) {
            w
        } else {
            return;
        };
        // TODO constrain size
        // let size = self.constrain_dim(w.bbox().size, applet_name);
        let size = w.bbox().size;
        let active_applet = if let Some(a) = self
            .active_applets
            .iter_mut()
            .find(|a| &a.s_wl_surface == s)
        {
            a
        } else {
            return;
        };

        let applet_name = &active_applet.name;
        // let activated = match w.toplevel() {
        //     Kind::Xdg(t) => t
        //         .current_state()
        //         .states
        //         .contains(xdg_toplevel::State::Activated),
        // };
        if active_applet.dimensions != size {
            if let Some((_, _)) = &self.output {
                // TODO improve this for when there are changes to the lists of plugins while running
                let pending_dimensions = active_applet
                    .pending_dimensions
                    .unwrap_or(active_applet.dimensions);
                let mut wait_configure_dim = active_applet
                    .next_render_event
                    .get()
                    .map(|e| match e {
                        SpaceEvent::Configure {
                            first,
                            width,
                            height,
                            serial: _serial,
                        } => (width, height),
                        SpaceEvent::WaitConfigure { width, height, .. } => (width, height),
                        _ => active_applet.dimensions.into(),
                    })
                    .unwrap_or(pending_dimensions.into());
                if active_applet.dimensions.w < size.w
                    && pending_dimensions.w < size.w
                    && wait_configure_dim.0 < size.w
                {
                    active_applet
                        .pending_dimensions
                        .replace((size.w, wait_configure_dim.1).into());
                    wait_configure_dim.0 = size.w;
                }
                if active_applet.dimensions.h < size.h
                    && pending_dimensions.h < size.h
                    && wait_configure_dim.1 < size.h
                {
                    active_applet
                        .pending_dimensions
                        .replace((wait_configure_dim.0, size.h).into());
                }
            } else {
                if active_applet
                    .next_render_event
                    .get()
                    .map(|e| match e {
                        SpaceEvent::Configure {
                            first,
                            width,
                            height,
                            serial: _serial,
                        } => (width, height).into(),
                        SpaceEvent::WaitConfigure { width, height, .. } => (width, height).into(),
                        _ => active_applet.dimensions,
                    })
                    .unwrap_or(
                        active_applet
                            .pending_dimensions
                            .unwrap_or(active_applet.dimensions),
                    )
                    != size
                {
                    active_applet.pending_dimensions.replace(size);
                    active_applet.full_clear = true;
                }
            }
        }
        self.space.commit(s);
    }

    fn dirty_popup(&mut self, dh: &DisplayHandle, s: &s_WlSurface) {
        self.space.commit(&s);
        self.space.refresh(&dh);
        self.last_dirty = Some(Instant::now());

        if let Some(p) = self
            .active_applets
            .iter_mut()
            .map(|a| &mut a.popups)
            .flatten()
            .find(|p| p.s_surface.wl_surface() == s)
        {
            p.dirty = true;
            self.popup_manager.commit(s);
        }
    }

    fn renderer(&mut self) -> Option<&mut Gles2Renderer> {
        self.renderer.as_mut()
    }

    fn visibility(&self) -> Visibility {
        if self.active_applets.is_empty() {
            Visibility::Hidden
        } else {
            Visibility::Visible
        }
    }

    fn keyboard_focus_lost(&mut self) {
        self.close_popups();
        self.active_applets.retain(|a| !a.config.hide_on_focus_loss);
        dbg!(self.active_applets.len());
    }
}

impl Drop for AppletHostSpace {
    fn drop(&mut self) {
        self.layer_surface.as_mut().map(|s| s.destroy());
        self.layer_shell_wl_surface.as_mut().map(|s| s.destroy());
    }
}
