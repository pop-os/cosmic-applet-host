// SPDX-License-Identifier: MPL-2.0-only

use std::{
    cell::{Cell, RefCell},
    default::Default,
    ffi::OsString,
    fs,
    os::{
        raw::c_int,
        unix::{net::UnixStream, prelude::AsRawFd},
    },
    process::Child,
    rc::Rc,
    time::Instant,
};

use anyhow::bail;
use freedesktop_desktop_entry::{self, DesktopEntry, Iter};
use itertools::{izip, Itertools};
use sctk::{
    compositor::{CompositorState, Region},
    output::OutputInfo,
    reexports::{
        client::protocol::{wl_output as c_wl_output, wl_surface as c_wl_surface},
        client::{protocol::wl_display::WlDisplay, Connection, Proxy, QueueHandle},
        protocols::xdg::shell::client::xdg_positioner::{Anchor, Gravity},
    },
    shell::{
        layer::{self, KeyboardInteractivity, LayerState, LayerSurface},
        xdg::popup,
    },
};

use slog::{info, trace, Logger};
use smithay::{
    backend::{
        egl::{
            context::{EGLContext, GlAttributes},
            display::EGLDisplay,
            ffi::{
                self,
                egl::{GetConfigAttrib, SwapInterval},
            },
            surface::EGLSurface,
        },
        renderer::{gles2::Gles2Renderer, utils::draw_surface_tree, Bind, Frame, Renderer, Unbind},
    },
    desktop::{
        draw_window,
        utils::{bbox_from_surface_tree, damage_from_surface_tree},
        Kind, PopupKind, PopupManager, Space, Window, WindowSurfaceType,
    },
    output::Output,
    reexports::{
        wayland_protocols_wlr::layer_shell::v1::client::{
            zwlr_layer_shell_v1::{self, Layer},
            zwlr_layer_surface_v1,
        },
        wayland_server::{
            self, protocol::wl_surface::WlSurface as s_WlSurface, Client, DisplayHandle, Resource,
        },
    },
    utils::{Logical, Physical, Rectangle, Size},
    wayland::shell::xdg::{PopupSurface, PositionerState},
};
use smithay::{
    desktop::space::RenderZindex,
    wayland::{
        compositor::{with_states, SurfaceAttributes},
        shell::xdg::SurfaceCachedState,
    },
};
use wayland_egl::WlEglSurface;
use xdg_shell_wrapper::{
    client_state::ClientFocus,
    server_state::{ServerFocus, ServerPointerFocus, ServerPtrFocus},
    shared_state::GlobalState,
    space::{
        ClientEglSurface, SpaceEvent, Visibility, WrapperPopup, WrapperPopupState, WrapperSpace,
    },
    util::{exec_child, get_client_sock},
};

use cosmic_applet_host_config::{AppletConfig, AppletHostConfig};

#[derive(Debug)]
pub struct ActiveApplet {
    /// XXX implicitly dropped first
    pub(crate) egl_surface: Option<Rc<EGLSurface>>,

    pub(crate) s_wl_surface: s_WlSurface,
    pub(crate) name: String,
    pub(crate) layer_surface: LayerSurface,
    pub(crate) w_accumulated_damage: Vec<Vec<Rectangle<i32, Physical>>>,
    pub(crate) popups: Vec<WrapperPopup>,
    // tracking for drawing
    pub(crate) pending_dimensions: Option<Size<i32, Logical>>,
    pub(crate) full_clear: u8,
    pub(crate) should_render: bool,
    pub(crate) space_event: Rc<Cell<Option<SpaceEvent>>>,
    pub(crate) dimensions: Size<i32, Logical>,
    pub(crate) config: AppletConfig,
    pub to_destroy: bool,
}

impl ActiveApplet {
    // Handles any events that have occurred since the last call, redrawing if needed. Returns true if the surface is alive.
    pub(crate) fn handle_events(
        &mut self,
        _log: Logger,
        _c_display: &WlDisplay,
        _renderer: &Gles2Renderer,
        _egl_display: &EGLDisplay,
    ) -> bool {
        self.should_render = false;
        match self.space_event.take() {
            Some(SpaceEvent::Quit) => {
                return false;
            }
            Some(SpaceEvent::WaitConfigure {
                first,
                width,
                height,
            }) => {
                self.space_event.replace(Some(SpaceEvent::WaitConfigure {
                    first,
                    width,
                    height,
                }));
            }
            None => {
                if let Some(d) = self.pending_dimensions.take() {
                    self.layer_surface
                        .set_size(d.w.try_into().unwrap(), d.h.try_into().unwrap());
                    self.layer_surface.wl_surface().commit();
                    self.space_event.replace(Some(SpaceEvent::WaitConfigure {
                        first: false,
                        width: d.w,
                        height: d.h,
                    }));
                    self.full_clear = 4;
                } else if self
                    .egl_surface
                    .as_ref()
                    .map(|egl_surface| {
                        egl_surface.get_size() != Some(self.dimensions.to_physical(1))
                    })
                    .unwrap_or(false)
                {
                    self.full_clear = 4;
                } else {
                    self.should_render = true;
                }
            }
        }
        true
    }
}

/// space for the cosmic panel
#[derive(Debug)]
pub struct AppletHostSpace {
    // XXX implicitly dropped first
    pub(crate) egl_surface: Option<Rc<EGLSurface>>,

    pub config: AppletHostConfig,
    pub log: Option<Logger>,

    pub(crate) space: Space,
    pub(crate) clients: Vec<(Client, String)>,
    pub(crate) children: Vec<Child>,
    pub(crate) last_dirty: Option<Instant>,
    /// focused surface so it can be changed when a window is removed
    pub(crate) c_focused_surface: Rc<RefCell<ClientFocus>>,
    pub(crate) c_hovered_surface: Rc<RefCell<ClientFocus>>,
    pub(crate) s_focused_surface: ServerFocus,
    pub(crate) s_hovered_surface: ServerPtrFocus,

    pub(crate) output: Option<(c_wl_output::WlOutput, Output, OutputInfo)>,
    pub(crate) display_handle: Option<DisplayHandle>,
    pub(crate) c_display: Option<WlDisplay>,
    pub(crate) egl_display: Option<EGLDisplay>,
    pub(crate) renderer: Option<Gles2Renderer>,
    pub(crate) layer_shell_wl_surface: Option<c_wl_surface::WlSurface>,
    pub(crate) layer_surface: Option<LayerSurface>,
    pub(crate) space_event: Rc<Cell<Option<SpaceEvent>>>,
    active_applets: Vec<ActiveApplet>,
    start: Instant,
}

impl AppletHostSpace {
    /// create a new space for the cosmic panel
    pub fn new(config: AppletHostConfig, log: Logger) -> Self {
        Self {
            config,
            space: Space::new(log.clone()),
            log: Some(log),
            clients: Default::default(),
            children: Default::default(),
            last_dirty: Default::default(),
            output: Default::default(),
            display_handle: Default::default(),
            c_display: Default::default(),
            egl_display: Default::default(),
            renderer: Default::default(),
            egl_surface: Default::default(),
            layer_shell_wl_surface: Default::default(),
            active_applets: Default::default(),
            start: Instant::now(),
            space_event: Rc::new(Cell::new(None)),
            layer_surface: None,
            c_focused_surface: Default::default(),
            c_hovered_surface: Default::default(),
            s_focused_surface: Default::default(),
            s_hovered_surface: Default::default(),
        }
    }

    fn close_popups(&mut self) {
        for w in &mut self.space.windows() {
            for (PopupKind::Xdg(p), _) in
                PopupManager::popups_for_surface(w.toplevel().wl_surface())
            {
                p.send_popup_done();
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

    fn render(&mut self, time: u32) -> anyhow::Result<()> {
        let clear_color = [0.0, 0.0, 0.0, 0.0];
        let renderer = if let Some(r) = self.renderer.as_mut() {
            r
        } else {
            return Ok(());
        };

        let log_clone = self.log.clone().unwrap();

        for active_applet in &mut self.active_applets {
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
            let output_size = o
                .current_mode()
                .ok_or_else(|| anyhow::anyhow!("failed to get current mode"))?
                .size;
            // TODO handle fractional scaling?
            // let output_scale = o.current_scale().fractional_scale();
            // We explicitly use ceil for the output geometry size to make sure the damage
            // spans at least the output size. Round and floor would result in parts not drawn as the
            // frame size could be bigger than the maximum the output_geo would define.
            let output_geo =
                Rectangle::from_loc_and_size(o.current_location(), output_size.to_logical(1));

            if active_applet.to_destroy {
                let _ = renderer.unbind();
                renderer.bind(active_applet.egl_surface.as_ref().unwrap().clone())?;
                let _ = renderer.render(
                    active_applet.dimensions.to_physical(1),
                    smithay::utils::Transform::Flipped180,
                    |_: &mut Gles2Renderer, frame| {
                        frame
                            .clear(
                                clear_color,
                                &[],
                            )
                            .expect("Failed to clear frame.");
                    }
                );
                continue;
            }

            else if active_applet.should_render {
                // TODO remove the permanent full clear after fixing the issue where resized windows aren't fully rendered
                let cur_damage = if active_applet.full_clear > 0 {
                    vec![]
                } else {
                    w.accumulated_damage(
                        w.geometry().loc.to_f64().to_physical(1.0),
                        1.0,
                        Some((&self.space, &o)),
                    )
                };

                let mut damage = match Self::damage_for_buffer(
                    cur_damage,
                    &mut active_applet.w_accumulated_damage,
                    &active_applet.egl_surface.as_ref().unwrap().clone(),
                    active_applet.full_clear,
                ) {
                    None => vec![],
                    Some(d) if d.is_empty() => continue,
                    Some(d) => d,
                };

                damage.dedup();
                damage.retain(|rect| rect.overlaps(output_geo.to_physical(1)));
                damage.retain(|rect| rect.size.h > 0 && rect.size.w > 0);

                let w_loc = self
                    .space
                    .window_location(&w)
                    .unwrap_or_else(|| (0, 0).into());

                let a_damage = if damage.is_empty() {
                    vec![Rectangle::from_loc_and_size(
                        w_loc.to_physical(1),
                        active_applet.dimensions.to_physical(1),
                    )]
                } else {
                    damage.clone()
                };
                let _ = renderer.unbind();
                renderer.bind(active_applet.egl_surface.as_ref().unwrap().clone())?;
                let _ = renderer.render(
                    active_applet.dimensions.to_physical(1),
                    smithay::utils::Transform::Flipped180,
                    |renderer: &mut Gles2Renderer, frame| {
                        frame
                            .clear(
                                clear_color,
                                a_damage.iter().cloned().collect_vec().as_slice(),
                            )
                            .expect("Failed to clear frame.");

                        let _ = draw_window(
                            renderer,
                            frame,
                            w,
                            1.0,
                            w_loc.to_physical(1).to_f64(),
                            &a_damage,
                            &log_clone,
                        );
                    },
                );
                let mut damage = damage
                    .iter()
                    .map(|rect| {
                        Rectangle::from_loc_and_size(
                            (
                                rect.loc.x,
                                active_applet.dimensions.h - rect.loc.y - rect.size.h,
                            ),
                            rect.size,
                        )
                    })
                    .collect::<Vec<_>>();
                active_applet
                    .egl_surface
                    .as_ref()
                    .unwrap()
                    .swap_buffers(if damage.is_empty() { None } else { None })?;
                active_applet.full_clear =
                    active_applet.full_clear.checked_sub(1).unwrap_or_default();
            }
        }

        for active_applet in &mut self.active_applets {
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
            // Popup rendering
            let clear_color = [0.0, 0.0, 0.0, 0.0];
            for p in active_applet
                .popups
                .iter_mut()
                .filter(|p| p.dirty && p.dirty && p.state.is_none())
            {
                let _ = renderer.unbind();
                renderer.bind(p.egl_surface.as_ref().unwrap().clone())?;
                let p_bbox = bbox_from_surface_tree(p.s_surface.wl_surface(), (0, 0));
                let cur_damage = if p.full_clear > 0 {
                    vec![]
                } else {
                    damage_from_surface_tree(
                        p.s_surface.wl_surface(),
                        p_bbox.loc.to_f64().to_physical(1.0),
                        1.0,
                        Some((&self.space, &o)),
                    )
                };

                let damage = match Self::damage_for_buffer(
                    cur_damage,
                    &mut p.accumulated_damage,
                    &p.egl_surface.as_ref().unwrap().clone(),
                    p.full_clear,
                ) {
                    None => vec![],
                    Some(d) if d.is_empty() => continue,
                    Some(d) => d,
                };

                let _ = renderer.render(
                    p_bbox.size.to_physical(1),
                    smithay::utils::Transform::Flipped180,
                    |renderer: &mut Gles2Renderer, frame| {
                        let p_damage = if damage.is_empty() {
                            vec![p_bbox.to_physical(1)]
                        } else {
                            damage.clone()
                        };

                        frame
                            .clear(
                                clear_color,
                                p_damage.iter().cloned().collect_vec().as_slice(),
                            )
                            .expect("Failed to clear frame.");

                        let _ = draw_surface_tree(
                            renderer,
                            frame,
                            p.s_surface.wl_surface(),
                            1.0,
                            p_bbox.loc.to_f64().to_physical(1.0),
                            &p_damage,
                            &log_clone,
                        );
                    },
                );
                let damage = damage
                    .iter()
                    .map(|rect| {
                        Rectangle::from_loc_and_size(
                            (rect.loc.x, p_bbox.size.h - rect.loc.y - rect.size.h),
                            rect.size,
                        )
                    })
                    .collect::<Vec<_>>();
                p.egl_surface
                    .as_ref()
                    .unwrap()
                    .swap_buffers(if damage.is_empty() { None } else { None })
                    .expect("Failed to swap buffers.");
                p.dirty = false;
                p.full_clear = p.full_clear.checked_sub(1).unwrap_or_default();
            }
        }
        let _ = renderer.unbind();

        self.space.send_frames(time);
        Ok(())
    }

    pub(crate) fn damage_for_buffer(
        cur_damage: Vec<Rectangle<i32, Physical>>,
        acc_damage: &mut Vec<Vec<Rectangle<i32, Physical>>>,
        egl_surface: &Rc<EGLSurface>,
        full_clear: u8,
    ) -> Option<Vec<Rectangle<i32, Physical>>> {
        let mut age: usize = egl_surface
            .buffer_age()
            .unwrap_or_default()
            .try_into()
            .unwrap_or_default();

        // reset accumulated damage when applying full clear for the first time
        if full_clear == 4 {
            acc_damage.drain(..);
        }

        let dmg_counts = acc_damage.len();
        // buffer contents undefined, treat as a full clear
        let ret = if age == 0 {
            acc_damage.drain(..);
            None
            // buffer older than we keep track of, full clear, but don't reset accumulated damage, instead add to acc damage
        } else if age >= dmg_counts {
            acc_damage.push(cur_damage);
            None
            // use get the accumulated damage for the last [age] renders, and add to acc damage
        } else {
            acc_damage.push(cur_damage);
            age += 1;
            let mut d = acc_damage.clone();
            d.reverse();
            let d = d[..age + 1]
                .iter()
                .flat_map(|v| v.iter().cloned())
                .collect_vec();
            Some(d)
        };

        // acc damage should only ever be length 4
        if acc_damage.len() > 4 {
            acc_damage.drain(..acc_damage.len() - 4);
        }

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

    pub fn hide_applet(
        &mut self,
        applet_name: &str,
    ) -> anyhow::Result<()> {
        // cleanup
        if let Some(i) = self
            .active_applets
            .iter()
            .position(|a| a.name == applet_name.to_string())
        {
            let _old_active_applet = &mut self.active_applets[i];
            _old_active_applet.to_destroy = true;
            return Ok(());
        }
        bail!("Applet is not active")
    }

    // TODO should this include the seat name that it should be toggled for?
    pub fn toggle_applet<W: WrapperSpace>(
        &mut self,
        applet_name: &str,
        compositor_state: &sctk::compositor::CompositorState,
        layer_state: &mut LayerState,
        qh: &QueueHandle<GlobalState<W>>,
    ) -> anyhow::Result<()> {
        // cleanup
        if self.hide_applet(applet_name).is_ok() { return Ok(()) };
        self.show_applet(applet_name, compositor_state, layer_state, qh)
    }

    pub fn show_applet<W: WrapperSpace>(
        &mut self,
        applet_name: &str,
        compositor_state: &sctk::compositor::CompositorState,
        layer_state: &mut LayerState,
        qh: &QueueHandle<GlobalState<W>>,
    )  -> anyhow::Result<()> {
        let c_surface = compositor_state.create_surface(qh)?;

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
            })
            .cloned();
        if let Some(w) = w {
            // TODO is this necessary?
            // self.s_focused_surface.replace(w.toplevel().wl_surface().clone());
            self.space.raise_window(&w, true);
            let s_wl_surface = w.toplevel().wl_surface();
            let dimensions = w.bbox().size;

            let layer = match self.config().layer(applet_name).unwrap() {
                zwlr_layer_shell_v1::Layer::Background => layer::Layer::Background,
                zwlr_layer_shell_v1::Layer::Bottom => layer::Layer::Bottom,
                zwlr_layer_shell_v1::Layer::Top => layer::Layer::Top,
                zwlr_layer_shell_v1::Layer::Overlay => layer::Layer::Overlay,
                _ => bail!("Invalid layer"),
            };

            let mut layer_surface_builder = LayerSurface::builder()
                .keyboard_interactivity(
                    match self.config.keyboard_interactivity(applet_name).unwrap() {
                        zwlr_layer_surface_v1::KeyboardInteractivity::None => {
                            KeyboardInteractivity::None
                        }
                        zwlr_layer_surface_v1::KeyboardInteractivity::Exclusive => {
                            KeyboardInteractivity::Exclusive
                        }
                        zwlr_layer_surface_v1::KeyboardInteractivity::OnDemand => {
                            KeyboardInteractivity::OnDemand
                        }
                        _ => bail!("Invalid keyboard interactivity"),
                    },
                )
                .size((
                    dimensions.w.try_into().unwrap(),
                    dimensions.h.try_into().unwrap(),
                ));

            if let Some((o, _, _)) = self.output.as_ref() {
                layer_surface_builder = layer_surface_builder.output(o);
            }
            let layer_surface =
                layer_surface_builder.map(qh, layer_state, c_surface.clone(), layer)?;
            layer_surface.set_anchor(match self.config.anchor(applet_name).unwrap() {
                cosmic_applet_host_config::Anchor::Left => layer::Anchor::LEFT,
                cosmic_applet_host_config::Anchor::Right => layer::Anchor::RIGHT,
                cosmic_applet_host_config::Anchor::Top => layer::Anchor::TOP,
                cosmic_applet_host_config::Anchor::Bottom => layer::Anchor::BOTTOM,
                cosmic_applet_host_config::Anchor::Center => layer::Anchor::empty(),
                cosmic_applet_host_config::Anchor::TopLeft => {
                    layer::Anchor::TOP.union(layer::Anchor::LEFT)
                }
                cosmic_applet_host_config::Anchor::TopRight => {
                    layer::Anchor::TOP.union(layer::Anchor::RIGHT)
                }
                cosmic_applet_host_config::Anchor::BottomLeft => {
                    layer::Anchor::BOTTOM.union(layer::Anchor::LEFT)
                }
                cosmic_applet_host_config::Anchor::BottomRight => {
                    layer::Anchor::BOTTOM.union(layer::Anchor::RIGHT)
                }
            });
            c_surface.commit();
            let next_render_event = Rc::new(Cell::new(Some(SpaceEvent::WaitConfigure {
                first: true,
                width: dimensions.w,
                height: dimensions.h,
            })));

            self.active_applets.push(ActiveApplet {
                name: applet_name.to_string(),
                layer_surface,
                egl_surface: None,
                w_accumulated_damage: vec![],
                s_wl_surface: s_wl_surface.clone(),
                popups: Default::default(),
                pending_dimensions: None,
                full_clear: 4,
                should_render: false,
                space_event: next_render_event,
                dimensions,
                config: self.config.applet(applet_name).unwrap().clone(),
                to_destroy: false,
            });
            self.space.raise_window(&w, true);
            return Ok(());
        }
        anyhow::bail!("No client with the requested name!");
    }
}

impl WrapperSpace for AppletHostSpace {
    type Config = AppletHostConfig;

    fn handle_events(
        &mut self,
        dh: &DisplayHandle,
        popup_manager: &mut PopupManager,
        time: u32,
    ) -> Instant {
        self.space.refresh(dh);
        popup_manager.cleanup();

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
        match self.space_event.take() {
            Some(SpaceEvent::Quit) => {
                // TODO cleanup
            }
            Some(SpaceEvent::WaitConfigure {
                first,
                width,
                height,
            }) => {
                self.space_event.replace(Some(SpaceEvent::WaitConfigure {
                    first,
                    width,
                    height,
                }));
            }
            None => {}
        }

        if let (Some(renderer), Some(egl_display), Some(_c_display)) = (
            self.renderer.as_ref(),
            self.egl_display.as_ref(),
            self.display_handle.as_ref(),
        ) {
            self.active_applets.retain_mut(|a| {
                a.popups.retain_mut(|p: &mut WrapperPopup| {
                    p.handle_events(
                        popup_manager,
                        renderer.egl_context(),
                        egl_display,
                        self.c_display.as_ref().unwrap(),
                    )
                });
                a.handle_events(
                    self.log.as_ref().unwrap().clone(),
                    self.c_display.as_ref().unwrap(),
                    self.renderer.as_ref().unwrap(),
                    self.egl_display.as_ref().unwrap(),
                )
            });
            let _ = self.render(time);
            self.active_applets.retain(|a| !a.to_destroy);
        }

        self.last_dirty.unwrap_or_else(|| self.start)
    }

    fn configure_layer(
        &mut self,
        layer: &LayerSurface,
        configure: sctk::shell::layer::LayerSurfaceConfigure,
    ) {
        if !layer.wl_surface().is_alive() {
            return;
        }
        let (w, h) = configure.new_size;
        if self.layer_surface.as_ref() == Some(layer) {
            match self.space_event.take() {
                Some(e) => match e {
                    SpaceEvent::WaitConfigure {
                        first,
                        mut width,
                        mut height,
                    } => {
                        if w != 0 {
                            width = w as i32;
                        }
                        if h != 0 {
                            height = h as i32;
                        }
                        if first {
                            let log = self.log.clone();
                            let client_egl_surface = unsafe {
                                ClientEglSurface::new(
                                    WlEglSurface::new(
                                        self.layer_shell_wl_surface.as_ref().unwrap().id(),
                                        width,
                                        height,
                                    )
                                    .unwrap(),
                                    self.c_display.as_ref().cloned().unwrap(),
                                    self.layer_shell_wl_surface.as_ref().unwrap().clone(),
                                )
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

                            let mut min_interval_attr = 23239;
                            unsafe {
                                GetConfigAttrib(
                                    egl_display.get_display_handle().handle,
                                    egl_context.config_id(),
                                    ffi::egl::MIN_SWAP_INTERVAL as c_int,
                                    &mut min_interval_attr,
                                );
                            }

                            let new_renderer = if let Some(renderer) = self.renderer.take() {
                                renderer
                            } else {
                                unsafe {
                                    Gles2Renderer::new(egl_context, log.clone())
                                        .expect("Failed to initialize EGL Surface")
                                }
                            };
                            if let Some(log) = log.as_ref().cloned() {
                                trace!(log, "{:?}", unsafe {
                                    SwapInterval(egl_display.get_display_handle().handle, 0)
                                });
                            }

                            let egl_surface = Rc::new(
                                EGLSurface::new(
                                    &egl_display,
                                    new_renderer
                                        .egl_context()
                                        .pixel_format()
                                        .expect("Failed to get pixel format from EGL context "),
                                    new_renderer.egl_context().config_id(),
                                    client_egl_surface,
                                    log.clone(),
                                )
                                .expect("Failed to initialize EGL Surface"),
                            );

                            self.renderer.replace(new_renderer);
                            self.egl_surface.replace(egl_surface);
                            self.egl_display.replace(egl_display);
                        }
                        self.egl_surface.as_ref().unwrap().resize(
                            width as i32,
                            height as i32,
                            0,
                            0,
                        );
                        self.layer_surface
                            .as_ref()
                            .unwrap()
                            .set_size(width as u32, height as u32);
                        self.layer_shell_wl_surface.as_ref().unwrap().commit();
                    }
                    SpaceEvent::Quit => (),
                },
                None => {
                    if w != 0 && h != 0 {
                        self.layer_surface
                            .as_ref()
                            .unwrap()
                            .set_size(w as u32, h as u32);
                        self.layer_shell_wl_surface.as_ref().unwrap().commit();
                    }
                }
            }
        } else if let Some(active_applet) = self
            .active_applets
            .iter_mut()
            .find(|a| &a.layer_surface == layer)
        {
            match active_applet.space_event.take() {
                Some(e) => match e {
                    SpaceEvent::WaitConfigure {
                        first,
                        mut width,
                        mut height,
                    } => {
                        if w != 0 {
                            width = w as i32;
                        }
                        if h != 0 {
                            height = h as i32;
                        }
                        if first {
                            let log = self.log.clone().unwrap();
                            let client_egl_surface = unsafe {
                                ClientEglSurface::new(
                                    WlEglSurface::new(
                                        active_applet.layer_surface.wl_surface().id(),
                                        width,
                                        height,
                                    )
                                    .unwrap(), // TODO remove unwrap
                                    self.c_display.as_ref().unwrap().clone(),
                                    active_applet.layer_surface.wl_surface().clone(),
                                )
                            };

                            let (new_renderer, egl_display) = if let (Some(renderer), Some(egl_display)) = (self.renderer.take(), self.egl_display.take()) {
                                (renderer, egl_display)
                            } else {
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
                                (renderer, egl_display)
                            };

                            let egl_surface = Rc::new(
                                EGLSurface::new(
                                    &egl_display,
                                    new_renderer
                                        .egl_context()
                                        .pixel_format()
                                        .expect("Failed to get pixel format from EGL context "),
                                    new_renderer.egl_context().config_id(),
                                    client_egl_surface,
                                    log.clone(),
                                )
                                .expect("Failed to initialize EGL Surface"),
                            );

                            self.renderer.replace(new_renderer);
                            active_applet.egl_surface.replace(egl_surface);
                            self.egl_display.replace(egl_display);
                        }
                        if active_applet.egl_surface.as_ref().unwrap().get_size()
                            != Some((width, height).into())
                            || active_applet.dimensions != (width as i32, height as i32).into()
                        {
                            active_applet.w_accumulated_damage.drain(..);
                            active_applet.dimensions = (width as i32, height as i32).into();
                            active_applet.egl_surface.as_ref().unwrap().resize(
                                width as i32,
                                height as i32,
                                0,
                                0,
                            );
                        }
                        active_applet.layer_surface.wl_surface().commit();
                        active_applet.full_clear = 4;
                    }
                    SpaceEvent::Quit => (),
                },
                None => {
                    if w != 0
                        && h != 0
                        && active_applet.dimensions != (w as i32, h as i32).into()
                        && active_applet.pending_dimensions.is_none()
                    {
                        active_applet.w_accumulated_damage.drain(..);
                        active_applet.dimensions = (w as i32, h as i32).into();
                        active_applet.layer_surface.wl_surface().commit();
                        active_applet.full_clear = 4;
                    }
                }
            }
        }
    }

    fn handle_press(&mut self, seat_name: &str) -> Option<s_WlSurface> {
        if let Some(f) = self.s_hovered_surface.iter().find_map(|h| {
            if h.seat_name.as_str() == seat_name {
                Some(h.surface.clone())
            } else {
                None
            }
        }) {
            Some(f)
        } else {
            self.keyboard_leave(seat_name, None);
            None
        }
    }

    fn add_window(&mut self, w: Window) {
        // TODO find active applet with matching client and full clear
        // self.full_clear = true;
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

    fn add_popup<W: WrapperSpace>(
        &mut self,
        compositor_state: &sctk::compositor::CompositorState,
        _conn: &sctk::reexports::client::Connection,
        qh: &QueueHandle<GlobalState<W>>,
        xdg_shell_state: &mut sctk::shell::xdg::XdgShellState,
        s_surface: PopupSurface,
        positioner: &sctk::shell::xdg::XdgPositioner,
        positioner_state: PositionerState,
    ) -> anyhow::Result<()> {
        let PositionerState {
            rect_size,
            anchor_rect,
            anchor_edges,
            gravity,
            constraint_adjustment,
            offset,
            reactive,
            parent_size,
            parent_configure: _,
        } = positioner_state;

        self.close_popups();

        let parent_window = if let Some(s) = self.space.windows().find(|w| match w.toplevel() {
            Kind::Xdg(wl_s) => Some(wl_s.wl_surface()) == s_surface.get_parent_surface().as_ref(),
        }) {
            s
        } else {
            bail!("could not find matching window");
        };

        let active_applet = if let Some(a) = self
            .active_applets
            .iter_mut()
            .find(|a| Some(&a.s_wl_surface) == s_surface.get_parent_surface().as_ref())
        {
            a
        } else {
            bail!("could not find matching active applet");
        };

        let c_wl_surface = compositor_state.create_surface(qh)?;

        let p_offset = self
            .space
            .window_location(parent_window)
            .unwrap_or_else(|| (0, 0).into());
        positioner.set_size(rect_size.w, rect_size.h);
        positioner.set_anchor_rect(
            anchor_rect.loc.x + p_offset.x,
            anchor_rect.loc.y + p_offset.y,
            anchor_rect.size.w,
            anchor_rect.size.h,
        );
        positioner.set_size(rect_size.w, rect_size.h);
        positioner.set_anchor_rect(
            anchor_rect.loc.x + parent_window.bbox().loc.x,
            anchor_rect.loc.y + parent_window.bbox().loc.y,
            anchor_rect.size.w,
            anchor_rect.size.h,
        );
        positioner.set_anchor(Anchor::try_from(anchor_edges as u32).unwrap_or(Anchor::None));
        positioner.set_gravity(Gravity::try_from(gravity as u32).unwrap_or(Gravity::None));

        positioner.set_constraint_adjustment(u32::from(constraint_adjustment));
        positioner.set_offset(offset.x, offset.y);
        if positioner.version() >= 3 {
            if reactive {
                positioner.set_reactive();
            }
            if let Some(parent_size) = parent_size {
                positioner.set_parent_size(parent_size.w, parent_size.h);
            }
        }
        let c_popup = popup::Popup::from_surface(
            None,
            positioner,
            qh,
            c_wl_surface.clone(),
            xdg_shell_state,
        )?;

        let input_region = Region::new(compositor_state)?;

        if let (Some(s_window_geometry), Some(input_regions)) =
            with_states(s_surface.wl_surface(), |states| {
                (
                    states.cached_state.current::<SurfaceCachedState>().geometry,
                    states
                        .cached_state
                        .current::<SurfaceAttributes>()
                        .input_region
                        .as_ref()
                        .cloned(),
                )
            })
        {
            c_popup.xdg_surface().set_window_geometry(
                s_window_geometry.loc.x,
                s_window_geometry.loc.y,
                s_window_geometry.size.w,
                s_window_geometry.size.h,
            );
            for r in input_regions.rects {
                input_region.add(0, 0, r.1.size.w, r.1.size.h);
            }
            c_wl_surface.set_input_region(Some(input_region.wl_region()));
        }

        active_applet.layer_surface.get_popup(c_popup.xdg_popup());

        //must be done after role is assigned as popup
        c_wl_surface.commit();

        let cur_popup_state = Some(WrapperPopupState::WaitConfigure);

        active_applet.popups.push(WrapperPopup {
            c_popup,
            c_wl_surface,
            s_surface,
            egl_surface: None,
            dirty: false,
            rectangle: Rectangle::from_loc_and_size((0, 0), (0, 0)),
            accumulated_damage: Default::default(),
            full_clear: 4,
            state: cur_popup_state,
            input_region,
        });
        Ok(())
    }

    fn reposition_popup(
        &mut self,
        popup: PopupSurface,
        _positioner: &sctk::shell::xdg::XdgPositioner,
        _positioner_state: PositionerState,
        token: u32,
    ) -> anyhow::Result<()> {
        popup.send_repositioned(token);
        popup.send_configure()?;
        Ok(())
    }

    fn config(&self) -> Self::Config {
        self.config.clone()
    }

    fn spawn_clients(
        &mut self,
        mut display: DisplayHandle,
    ) -> Result<Vec<UnixStream>, anyhow::Error> {
        if self.children.is_empty() {
            let (clients, sockets): (Vec<_>, Vec<_>) = self
                .config
                .applets
                .iter()
                .map(|a| {
                    let (c, s) = get_client_sock(&mut display);
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
                                            self.log.as_ref().unwrap().clone(),
                                            client_socket.as_raw_fd(),
                                            vec![],
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

    fn handle_output<W: WrapperSpace>(
        &mut self,
        compositor_state: &sctk::compositor::CompositorState,
        layer_state: &mut LayerState,
        _conn: &sctk::reexports::client::Connection,
        qh: &QueueHandle<GlobalState<W>>,
        c_output: Option<c_wl_output::WlOutput>,
        s_output: Option<Output>,
        output_info: Option<OutputInfo>,
    ) -> anyhow::Result<()> {
        if let (Some(_), Some(s_output), Some(output_info)) =
            (c_output.as_ref(), s_output.as_ref(), output_info.as_ref())
        {
            self.space.map_output(s_output, output_info.location);
            if self.config.output.as_ref() != output_info.name.as_ref() {
                bail!("output does not match config");
            }
        } else {
            if self.config.output.as_ref() != None {
                bail!("output does not match config");
            }
        }

        let dimensions: Size<i32, Physical> = (1, 1).into();
        if self.output.is_some() {
            bail!("output already added!")
        }

        let c_surface = compositor_state.create_surface(qh).unwrap();
        let mut layer_surface_builder = LayerSurface::builder()
            .keyboard_interactivity(KeyboardInteractivity::None)
            .size((
                dimensions.w.try_into().unwrap(),
                dimensions.h.try_into().unwrap(),
            ));
        if let Some(output) = c_output.as_ref() {
            layer_surface_builder = layer_surface_builder.output(output);
        }
        let layer_surface = layer_surface_builder
            .map(qh, layer_state, c_surface.clone(), layer::Layer::Background)
            .unwrap();

        layer_surface.set_anchor(layer::Anchor::BOTTOM);
        layer_surface.set_size(dimensions.w as u32, dimensions.h as u32);

        // Commit so that the server will send a configure event
        c_surface.commit();
        //let egl_surface_clone = egl_surface.clone();

        let next_render_event = Rc::new(Cell::new(Some(SpaceEvent::WaitConfigure {
            first: true,
            width: dimensions.w,
            height: dimensions.h,
        })));

        self.output = izip!(c_output.into_iter(), s_output.into_iter(), output_info).next();
        self.space_event = next_render_event;
        self.layer_shell_wl_surface.replace(c_surface);
        self.layer_surface.replace(layer_surface);
        Ok(())
    }

    fn log(&self) -> Option<Logger> {
        self.log.clone()
    }

    fn destroy(&mut self) {
        // TODO look more into this
        // for a in &mut self.active_applets {
        //     a.layer_shell_wl_surface.destroy();
        // }
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

        // let applet_name = &active_applet.name;
        // let activated = match w.toplevel() {
        //     Kind::Xdg(t) => t
        //         .current_state()
        //         .states
        //         .contains(xdg_toplevel::State::Activated),
        // };
        if active_applet.dimensions != size {
            if let Some((_, _, _)) = &self.output {
                // TODO improve this for when there are changes to the lists of plugins while running
                let pending_dimensions = active_applet
                    .pending_dimensions
                    .unwrap_or(active_applet.dimensions);
                let mut wait_configure_dim = active_applet
                    .space_event
                    .get()
                    .map(|e| match e {
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
                    .space_event
                    .get()
                    .map(|e| match e {
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
                    active_applet.full_clear = 4;
                }
            }
        }
        self.space.commit(s);
    }

    fn dirty_popup(&mut self, dh: &DisplayHandle, s: &s_WlSurface) {
        self.last_dirty = Some(Instant::now());
        self.space.commit(s);
        self.space.refresh(dh);

        if let Some(p) = self
            .active_applets
            .iter_mut()
            .map(|a| &mut a.popups)
            .flatten()
            .find(|p| p.s_surface.wl_surface() == s)
        {
            let p_bbox = bbox_from_surface_tree(p.s_surface.wl_surface(), (0, 0));
            let p_geo = PopupKind::Xdg(p.s_surface.clone()).geometry();
            if p_bbox != p.rectangle {
                p.c_popup.xdg_surface().set_window_geometry(
                    p_geo.loc.x,
                    p_geo.loc.y,
                    p_geo.size.w,
                    p_geo.size.h,
                );
                if let Some(input_regions) = with_states(p.s_surface.wl_surface(), |states| {
                    states
                        .cached_state
                        .current::<SurfaceAttributes>()
                        .input_region
                        .as_ref()
                        .cloned()
                }) {
                    p.input_region.subtract(
                        p_bbox.loc.x,
                        p_bbox.loc.y,
                        p_bbox.size.w,
                        p_bbox.size.h,
                    );
                    for r in input_regions.rects {
                        p.input_region
                            .add(r.1.loc.x, r.1.loc.y, r.1.size.w, r.1.size.h);
                    }
                    p.c_wl_surface
                        .set_input_region(Some(p.input_region.wl_region()));
                }
                p.state.replace(WrapperPopupState::Rectangle {
                    x: p_bbox.loc.x,
                    y: p_bbox.loc.y,
                    width: p_bbox.size.w,
                    height: p_bbox.size.h,
                });
            }
            p.dirty = true;
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

    fn set_display_handle(&mut self, display: wayland_server::DisplayHandle) {
        self.display_handle.replace(display);
    }

    fn setup<W: WrapperSpace>(
        &mut self,
        compositor_state: &CompositorState,
        layer_state: &mut LayerState,
        conn: &Connection,
        qh: &QueueHandle<GlobalState<W>>,
    ) {
        self.c_display.replace(conn.display());
        let _ = self.handle_output(compositor_state, layer_state, conn, qh, None, None, None);

        // self.handle_output(dh.clone(), &env, None, None, None)
        // .unwrap();
    }

    fn keyboard_leave(&mut self, seat_name: &str, c_s: Option<c_wl_surface::WlSurface>) {
        if self.s_focused_surface.iter().any(|f| f.1 == seat_name) {
            self.close_popups();
            self.active_applets.retain(|a| {
                !a.config.hide_on_focus_loss
                    || (!matches!(c_s, None) && Some(a.layer_surface.wl_surface()) != c_s.as_ref())
                    || self
                        .s_focused_surface
                        .iter()
                        .any(|f| f.1 != seat_name || a.s_wl_surface != f.0)
            });
            self.s_focused_surface.retain(|f| f.1 != seat_name);
        }
    }

    // TODO multi seat handling?

    fn keyboard_enter(
        &mut self,
        seat_name: &str,
        surface: c_wl_surface::WlSurface,
    ) -> Option<s_WlSurface> {
        self.active_applets.iter().find_map(|a| {
            if a.layer_surface.wl_surface() == &surface {
                let prev_kbd = self.s_focused_surface.iter_mut().find(|f| f.1 == seat_name);
                if let Some(prev_kbd) = prev_kbd {
                    prev_kbd.0 = a.s_wl_surface.clone();
                } else {
                    self.s_focused_surface
                        .push((a.s_wl_surface.clone(), seat_name.to_string()));
                }
                Some(a.s_wl_surface.clone())
            } else {
                None
            }
        })
    }

    fn pointer_leave(&mut self, seat_name: &str, _surface: Option<c_wl_surface::WlSurface>) {
        self.s_hovered_surface
            .retain(|focus| focus.seat_name != seat_name);
    }

    fn pointer_enter(
        &mut self,
        dim: (i32, i32),
        seat_name: &str,
        surface: c_wl_surface::WlSurface,
    ) -> Option<xdg_shell_wrapper::server_state::ServerPointerFocus> {
        self.update_pointer(dim, seat_name, surface)
    }

    // TODO fix this
    ///  update active window based on pointer location
    fn update_pointer(
        &mut self,
        (x, y): (i32, i32),
        seat_name: &str,
        surface: c_wl_surface::WlSurface,
    ) -> Option<xdg_shell_wrapper::server_state::ServerPointerFocus> {
        let mut prev_hover = self
            .s_hovered_surface
            .iter_mut()
            .enumerate()
            .find(|(_, f)| f.seat_name == seat_name);
        let prev_kbd = self.s_focused_surface.iter_mut().find(|f| f.1 == seat_name);

        // first check if the motion is on a popup's client surface
        if let Some(p) = self
            .active_applets
            .iter()
            .map(|a| a.popups.iter())
            .flatten()
            .find(|p| &p.c_wl_surface == &surface)
        {
            let geo = smithay::desktop::PopupKind::Xdg(p.s_surface.clone()).geometry();
            // special handling for popup bc they exist on their own client surface

            if let Some(prev_kbd) = prev_kbd {
                prev_kbd.0 = p.s_surface.wl_surface().clone();
            } else {
                self.s_focused_surface
                    .push((p.s_surface.wl_surface().clone(), seat_name.to_string()));
            }
            if let Some((_, prev_foc)) = prev_hover.as_mut() {
                prev_foc.c_pos = p.rectangle.loc.into();
                prev_foc.s_pos = p.rectangle.loc - geo.loc;

                prev_foc.surface = p.s_surface.wl_surface().clone();
                Some(prev_foc.clone())
            } else {
                self.s_hovered_surface.push(ServerPointerFocus {
                    surface: p.s_surface.wl_surface().clone(),
                    seat_name: seat_name.to_string(),
                    c_pos: p.rectangle.loc.into(),
                    s_pos: p.rectangle.loc - geo.loc,
                });
                self.s_hovered_surface.last().cloned()
            }
        } else {
            // if not on this panel's client surface exit
            if !self
                .active_applets
                .iter()
                .any(|a| a.layer_surface.wl_surface() == &surface)
            {
                return None;
            }
            if let Some((w, s, p)) = self
                .space
                .surface_under((x as f64, y as f64), WindowSurfaceType::ALL)
            {
                if let Some(prev_kbd) = prev_kbd {
                    prev_kbd.0 = w.toplevel().wl_surface().clone();
                } else {
                    self.s_focused_surface
                        .push((w.toplevel().wl_surface().clone(), seat_name.to_string()));
                }
                if let Some((_, prev_foc)) = prev_hover.as_mut() {
                    prev_foc.s_pos = p;
                    prev_foc.c_pos = w.geometry().loc;
                    prev_foc.surface = s.clone();
                    Some(prev_foc.clone())
                } else {
                    self.s_hovered_surface.push(ServerPointerFocus {
                        surface: s,
                        seat_name: seat_name.to_string(),
                        c_pos: w.geometry().loc,
                        s_pos: (x, y).into(),
                    });
                    self.s_hovered_surface.last().cloned()
                }
            } else {
                if let Some((prev_i, _)) = prev_hover {
                    self.s_hovered_surface.swap_remove(prev_i);
                }
                None
            }
        }
    }

    fn output_leave(
        &mut self,
        _c_output: Option<c_wl_output::WlOutput>,
        _s_output: Option<Output>,
        _info: Option<OutputInfo>,
    ) -> anyhow::Result<()> {
        bail!("TODO")
    }

    fn configure_popup(
        &mut self,
        _popup: &sctk::shell::xdg::popup::Popup,
        _config: sctk::shell::xdg::popup::PopupConfigure,
    ) {
        // TODO
    }

    fn close_popup(&mut self, popup: &sctk::shell::xdg::popup::Popup) {
        if let Some(p) = self.active_applets.iter().find_map(|a| {
            a.popups
                .iter()
                .find(|p| &p.c_wl_surface == popup.wl_surface())
        }) {
            p.s_surface.send_popup_done();
        }
    }

    fn close_layer(&mut self, layer: &sctk::shell::layer::LayerSurface) {
        if self.layer_shell_wl_surface.as_ref() == Some(layer.wl_surface()) {
            self.egl_surface.take();
            self.layer_surface.take();
            self.layer_shell_wl_surface.take();
        } else {
            self.active_applets
                .retain(|a| a.layer_surface.wl_surface() != layer.wl_surface())
        }
    }

    fn get_client_focused_surface(&self) -> Rc<RefCell<ClientFocus>> {
        self.c_focused_surface.clone()
    }

    fn get_client_hovered_surface(&self) -> Rc<RefCell<ClientFocus>> {
        self.c_hovered_surface.clone()
    }
}
