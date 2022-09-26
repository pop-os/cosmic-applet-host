// SPDX-License-Identifier: MPL-2.0-only

use anyhow::Result;
use launch_pad::process::Process;
use launch_pad::ProcessManager;
use slog::{o, Drain};
use smithay::reexports::calloop;
use smithay::reexports::wayland_server::backend::ClientId;
use smithay::reexports::wayland_server::Client;
use std::os::unix::net::UnixStream;
use tokio::runtime;
use tokio::sync::mpsc;
use xdg_shell_wrapper::client_state::ClientState;
use xdg_shell_wrapper::{run, shared_state::GlobalState};
use zbus::blocking::ConnectionBuilder;

use crate::calloop::channel::Event;
use crate::dbus::AppletHostServer;
use crate::space::AppletHostSpace;

mod dbus;
mod space;

#[derive(Debug)]
pub(crate) enum AppletHostEvent {
    ToggleName(String),
    HideName(String),
    ShowName(String),
    ClientSocketPair(String, ClientId, Client, UnixStream),
}

pub enum AppletMsg {
    NewProcess(Process),
    ClientSocketPair(String, ClientId, Client, UnixStream),
}

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

    let (applet_tx, mut applet_rx) = mpsc::channel(200);
    let (unpause_launchpad_tx, unpause_launchpad_rx) = std::sync::mpsc::sync_channel(200);

    let (calloop_tx, calloop_rx) = calloop::channel::sync_channel(100);
    let calloop_tx_clone = calloop_tx.clone();
    std::thread::spawn(move || -> anyhow::Result<()> {
        let done = event_listener::Event::new();
        let dbus_server = AppletHostServer {
            tx: calloop_tx_clone,
            done,
        };
        let done_listener = dbus_server.done.listen();
        let _ = ConnectionBuilder::session()?
            .name("com.system76.CosmicAppletHost")?
            .serve_at("/com/system76/CosmicAppletHost", dbus_server)?
            .build()?;

        done_listener.wait();
        Ok(())
    });

    std::thread::spawn(move || -> anyhow::Result<()> {
        let rt = runtime::Builder::new_current_thread()
            .enable_all()
            .build()?;
        rt.block_on(async {
            let process_manager = ProcessManager::new().await;
            let _ = process_manager.set_max_restarts(10);
            while let Some(msg) = applet_rx.recv().await {
                match msg {
                    AppletMsg::NewProcess(process) => {
                        let _ = process_manager.start(process).await;
                    }
                    AppletMsg::ClientSocketPair(id, client_id, c, s) => {
                        let _ =
                            calloop_tx.send(AppletHostEvent::ClientSocketPair(id, client_id, c, s));
                        // XXX This is done to avoid a possible race,
                        // the client & socket need to be update in the panel_space state before the process starts again
                        let _ = unpause_launchpad_rx.recv();
                    }
                };
            }
        });

        Ok(())
    });

    event_loop
        .handle()
        .insert_source(
            calloop_rx,
            move |e, _, state: &mut GlobalState<AppletHostSpace>| {
                let GlobalState {
                    space,
                    client_state:
                        ClientState {
                            queue_handle,
                            compositor_state,
                            layer_state,
                            ..
                        },
                    ..
                } = state;
                match e {
                    Event::Msg(AppletHostEvent::ToggleName(name)) => {
                        let _ = space.toggle_applet(
                            &name,
                            &compositor_state,
                            layer_state,
                            queue_handle,
                        );
                    }
                    Event::Msg(AppletHostEvent::HideName(name)) => {
                        let _ = space.hide_applet(&name);
                    }
                    Event::Msg(AppletHostEvent::ShowName(name)) => {
                        let _ =
                            space.show_applet(&name, &compositor_state, layer_state, queue_handle);
                    }
                    Event::Msg(AppletHostEvent::ClientSocketPair(id, client_id, c, s)) => {
                        state.space.replace_client(id, client_id, c, s);
                        unpause_launchpad_tx
                            .try_send(())
                            .expect("Failed to unblock launchpad");
                    }
                    Event::Closed => {
                        // TODO gracefully shut down
                        todo!()
                    }
                }
            },
        )
        .expect("failed to insert dbus event source");
    run(AppletHostSpace::new(config, log, applet_tx), event_loop)?;
    Ok(())
}
