// SPDX-License-Identifier: MPL-2.0-only

use std::collections::HashMap;
use std::fs::File;
use std::ops::Range;

use anyhow::Result;
use serde::{Deserialize, Serialize};
use wayland_protocols::wlr::unstable::layer_shell::v1::client::{
    zwlr_layer_shell_v1, zwlr_layer_surface_v1,
};
use xdg::BaseDirectories;
use xdg_shell_wrapper_config::{KeyboardInteractivity, Layer, WrapperConfig};

/// Edge to which the panel is anchored
#[derive(Debug, Deserialize, Serialize, Copy, Clone)]
pub enum Anchor {
    /// anchored to left edge
    Left,
    /// anchored to right edge
    Right,
    /// anchored to top edge
    Top,
    /// anchored to bottom edge
    Bottom,
    ///
    Center,
    ///
    TopLeft,
    ///
    TopRight,
    ///
    BottomLeft,
    ///
    BottomRight,
}

impl Default for Anchor {
    fn default() -> Self {
        Anchor::Top
    }
}

impl From<zwlr_layer_surface_v1::Anchor> for Anchor {
    fn from(align: zwlr_layer_surface_v1::Anchor) -> Self {
        if align.contains(zwlr_layer_surface_v1::Anchor::Left) {
            Self::Left
        } else if align.contains(zwlr_layer_surface_v1::Anchor::Right) {
            Self::Right
        } else if align.contains(zwlr_layer_surface_v1::Anchor::Top) {
            Self::Top
        } else if align.contains(zwlr_layer_surface_v1::Anchor::Bottom) {
            Self::Bottom
        } else if align.contains(zwlr_layer_surface_v1::Anchor::Top)
            && align.contains(zwlr_layer_surface_v1::Anchor::Left)
        {
            Self::TopLeft
        } else if align.contains(zwlr_layer_surface_v1::Anchor::Top)
            && align.contains(zwlr_layer_surface_v1::Anchor::Right)
        {
            Self::TopRight
        } else if align.contains(zwlr_layer_surface_v1::Anchor::Bottom)
            && align.contains(zwlr_layer_surface_v1::Anchor::Left)
        {
            Self::BottomLeft
        } else if align.contains(zwlr_layer_surface_v1::Anchor::Bottom)
            && align.contains(zwlr_layer_surface_v1::Anchor::Right)
        {
            Self::BottomRight
        } else {
            Self::Center
        }
    }
}

impl Into<zwlr_layer_surface_v1::Anchor> for Anchor {
    fn into(self) -> zwlr_layer_surface_v1::Anchor {
        let mut anchor = zwlr_layer_surface_v1::Anchor::empty();
        match self {
            Self::Left => {
                anchor.insert(zwlr_layer_surface_v1::Anchor::Left);
            }
            Self::Right => {
                anchor.insert(zwlr_layer_surface_v1::Anchor::Right);
            }
            Self::Top => {
                anchor.insert(zwlr_layer_surface_v1::Anchor::Top);
            }
            Self::Bottom => {
                anchor.insert(zwlr_layer_surface_v1::Anchor::Bottom);
            }
            Anchor::TopLeft => {
                anchor.insert(zwlr_layer_surface_v1::Anchor::Top);
                anchor.insert(zwlr_layer_surface_v1::Anchor::Left)
            }
            Anchor::TopRight => {
                anchor.insert(zwlr_layer_surface_v1::Anchor::Top);
                anchor.insert(zwlr_layer_surface_v1::Anchor::Right)
            }
            Anchor::BottomLeft => {
                anchor.insert(zwlr_layer_surface_v1::Anchor::Bottom);
                anchor.insert(zwlr_layer_surface_v1::Anchor::Left)
            }
            Anchor::BottomRight => {
                anchor.insert(zwlr_layer_surface_v1::Anchor::Bottom);
                anchor.insert(zwlr_layer_surface_v1::Anchor::Right)
            }
            Anchor::Center => {}
        };
        anchor
    }
}

/// configurable autohide behavior
#[derive(Debug, Deserialize, Serialize, Clone)]
pub struct Transition {
    /// time without pointer focus before hiding
    wait_time: u32,
    /// time that it should take to transition
    transition_time: u32,
    /// size of the handle in pixels
    /// should be > 0
    handle_size: u32,
}

#[derive(Debug, Deserialize, Serialize, Clone)]
pub struct AppletConfig {
    pub name: String,
    pub anchor: Anchor,
    pub layer: Layer,
    pub keyboard_interactivity: KeyboardInteractivity,
    pub width: Option<Range<u32>>,
    pub height: Option<Range<u32>>,
    pub transition: Option<Transition>,
    pub hide_shortcuts: Vec<String>,
    pub hide_on_focus_loss: bool,
}

impl Default for AppletConfig {
    fn default() -> Self {
        Self {
            name: "".to_string(),
            anchor: Anchor::Center,
            layer: Layer::Top,
            keyboard_interactivity: KeyboardInteractivity::OnDemand,
            width: None,
            height: None,
            transition: Some(Transition {
                wait_time: 1000,
                transition_time: 200,
                handle_size: 4,
            }),
            hide_shortcuts: vec![],
            hide_on_focus_loss: true,
        }
    }
}

impl AppletConfig {
    /// get constraints for the dimensions
    pub fn dimensions(&self) -> (Option<Range<u32>>, Option<Range<u32>>) {
        (self.width.clone(), self.height.clone())
    }

    fn layer(&self) -> zwlr_layer_shell_v1::Layer {
        self.layer.into()
    }

    fn keyboard_interactivity(&self) -> zwlr_layer_surface_v1::KeyboardInteractivity {
        self.keyboard_interactivity.into()
    }

    pub fn applet(&self) -> String {
        self.name.clone()
    }
}

#[derive(Debug, Deserialize, Serialize, Clone)]
pub struct AppletHostConfig {
    pub name: String,
    pub output: Option<String>,
    pub applets: Vec<AppletConfig>,
}

impl Default for AppletHostConfig {
    fn default() -> Self {
        let mut app_library_applet = AppletConfig::default();
        app_library_applet.name = "com.system76.CosmicAppLibrary".into();
        let mut launcher_applet = AppletConfig::default();
        launcher_applet.name = "com.system76.CosmicLauncher".into();
        Self {
            name: "".to_string(),
            applets: vec![app_library_applet, launcher_applet],
            output: None,
        }
    }
}

impl AppletHostConfig {
    pub fn load(name: &str) -> Self {
        match Self::configs().remove(name.into()) {
            Some(c) => c,
            _ => Self::default(),
        }
    }

    pub fn write(&self, name: &str) -> Result<()> {
        let mut configs = Self::configs();
        configs.insert(name.into(), AppletHostConfig::default());
        let xdg = BaseDirectories::new()?;
        let f = xdg.place_config_file("cosmic-applet-host/config.ron").unwrap();
        let f = File::create(f)?;
        ron::ser::to_writer_pretty(&f, &configs, ron::ser::PrettyConfig::default())?;
        return Ok(());
    }

    fn configs() -> HashMap<String, Self> {
        match BaseDirectories::new()
            .map(|dirs| dirs.find_config_file("cosmic-applet-host/config.ron"))
            .map(|c| c.map(|c| File::open(c)))
            .map(|file| file.map(|file| ron::de::from_reader::<_, HashMap<String, Self>>(file?)))
        {
            Ok(Some(Ok(c))) => c,
            _ => HashMap::new(),
        }
    }

    pub fn applet(&self, applet: &str) -> Option<&AppletConfig> {
        self.applets.iter().find(|a| &a.name == applet)
    }

    pub fn dimensions(&self, applet: &str) -> Option<(Option<Range<u32>>, Option<Range<u32>>)> {
        self.applets.iter().find_map(|a| {
            if &a.name == applet {
                Some(a.dimensions())
            } else {
                None
            }
        })
    }

    pub fn anchor(&self, applet: &str) -> Option<Anchor> {
        self.applets.iter().find_map(|a| {
            if &a.name == applet {
                Some(a.anchor)
            } else {
                None
            }
        })
    }

    pub fn layer(&self, applet: &str) -> Option<zwlr_layer_shell_v1::Layer> {
        self.applets.iter().find_map(|a| {
            if &a.name == applet {
                Some(a.layer())
            } else {
                None
            }
        })
    }

    pub fn keyboard_interactivity(
        &self,
        applet: &str,
    ) -> Option<zwlr_layer_surface_v1::KeyboardInteractivity> {
        self.applets.iter().find_map(|a| {
            if &a.name == applet {
                Some(a.keyboard_interactivity())
            } else {
                None
            }
        })
    }
}

impl WrapperConfig for AppletHostConfig {
    fn output(&self) -> Option<String> {
        self.output.clone()
    }

    fn name(&self) -> &str {
        &self.name
    }
}
