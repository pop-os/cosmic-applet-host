// SPDX-License-Identifier: MPL-2.0-only

use anyhow::Result;
use slog::{o, Drain};
use smithay::reexports::calloop;
use xdg_shell_wrapper::{run, shared_state::GlobalState};
use zbus::blocking::ConnectionBuilder;

use crate::calloop::channel::Event;
use crate::dbus::AppletHostServer;
use crate::{dbus::AppletHostEvent, space::AppletHostSpace};

mod dbus;
mod space;

fn main() -> Result<()> {
    // A logger facility, here we use the terminal
    let log = slog::Logger::root(
        slog_async::Async::default(slog_term::term_full().fuse()).fuse(),
        o!(),
    );

    let _guard = slog_scope::set_global_logger(log.clone());
    slog_stdlog::init().expect("Could not setup log backend");

    let arg = std::env::args().nth(1);
    let usage = "USAGE: cosmic-applet-host OR cosmic-applet-host <profile name>";
    let config = match arg.as_ref().map(|s| &s[..]) {
        Some(arg) if arg == "--help" || arg == "-h" => {
            println!("{}", usage);
            std::process::exit(1);
        }
        Some(profile) => cosmic_applet_host_config::AppletHostConfig::load(profile),
        None => Default::default(),
    };

    let event_loop = calloop::EventLoop::try_new()?;
    let (tx, rx) = calloop::channel::sync_channel(100);
    std::thread::spawn(move || -> anyhow::Result<()> {
        let done = event_listener::Event::new();
        let dbus_server = AppletHostServer { tx, done };
        let done_listener = dbus_server.done.listen();
        let _ = ConnectionBuilder::session()?
            .name("com.system76.CosmicAppletHost")?
            .serve_at("/com/system76/CosmicAppletHost", dbus_server)?
            .build()?;

        done_listener.wait();
        Ok(())
    });

    event_loop
        .handle()
        .insert_source(
            rx,
            |e,
             _,
             state: &mut GlobalState<AppletHostSpace>| {
                match e {
                    Event::Msg(AppletHostEvent::Name(name)) => {
                        let GlobalState {
                            space,
                            client_state,
                            ..
                        } = state;
                        let env_handle = &client_state.env_handle;
                        let _ = space.toggle_applet(&name, &env_handle);
                    }
                    Event::Closed => {
                        // TODO gracefully shut down
                        todo!()
                    }
                }
            },
        )
        .expect("failed to insert dbus event source");
    run(AppletHostSpace::new(config, log), event_loop)?;
    Ok(())
}
