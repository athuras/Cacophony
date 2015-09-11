extern crate portaudio;
extern crate portmidi as pm;

use portaudio::pa;
use std::error::Error;


fn main() {
    println!("Hello, world!");
    test_portaudio();
    test_portmidi();
    println!("Goodnight, moon.")
}

fn test_portaudio() {
    match pa::initialize() {
      Ok(())   => println!("Initialized Portaudio"),
      Err(err) => println!("Encountered error while initializing PortAudio: {}", err.description()),
    }
    let api_count = pa::host::get_api_count();
    println!("PortAudio host count : {}", api_count as isize);

    match pa::terminate() {
        Ok(())   => println!("Terminated PortAudio."),
        Err(err) => println!("Encountered error while terminating PortAudio: {}", err.description()),
    }
}

fn test_portmidi() {
    match pm::initialize() {
      Ok(())   => println!("Initialized PortMidi."),
      Err(err) => println!("Encountered error while initializing PortMidi: {:?}", err),
    }

    println!("Id  Name                 Input? Output?");
    for d in (0..pm::count_devices())
        .filter_map(|i| pm::get_device_info(i)) {

        println!("{:<3} {:<20} {:<6} {:<6}", d.device_id, d.name, d.input, d.output);
    }
    match pm::terminate() {
        Ok(())   => println!("Terminated PortMidi."),
        Err(err) => println!("Encountered error whilte terminating PortMidi: {:?}", err),
    }
}

