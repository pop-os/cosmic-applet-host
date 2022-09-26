use smithay::reexports::calloop::channel::SyncSender;
use zbus::dbus_interface;

use event_listener::Event;

use crate::AppletHostEvent;

#[derive(Debug)]
pub(crate) struct AppletHostServer {
    pub(crate) tx: SyncSender<AppletHostEvent>,
    pub(crate) done: Event,
}

#[dbus_interface(name = "com.system76.CosmicAppletHost")]
impl AppletHostServer {
    fn toggle(&self, name: &str) {
        let _ = self.tx.send(AppletHostEvent::ToggleName(name.to_string()));
    }

    fn hide(&self, name: &str) {
        let _ = self.tx.send(AppletHostEvent::HideName(name.to_string()));
    }

    fn show(&self, name: &str) {
        let _ = self.tx.send(AppletHostEvent::ShowName(name.to_string()));
    }

    fn done(&self) {
        self.done.notify(1);
    }
}
