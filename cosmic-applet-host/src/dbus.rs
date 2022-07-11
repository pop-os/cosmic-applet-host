use smithay::reexports::calloop::channel::SyncSender;
use std::error::Error;
use zbus::{
    blocking::{ConnectionBuilder, ObjectServer},
    dbus_interface, fdo, SignalContext,
};

use event_listener::Event;

#[derive(Debug)]
pub(crate) enum AppletHostEvent {
    Name(String),
}

#[derive(Debug)]
pub(crate) struct AppletHostServer {
    pub(crate) tx: SyncSender<AppletHostEvent>,
    pub(crate) done: Event,
}

#[dbus_interface(name = "com.system76.CosmicAppletHost")]
impl AppletHostServer {
    fn toggle(&self, name: &str) {
        let _ = self.tx.send(AppletHostEvent::Name(name.to_string()));
    }

    fn done(&self) {
        self.done.notify(1);
    }
}
