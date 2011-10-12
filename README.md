Asterisk module for Zotonic
===========================

This module enables you to control Asterisk from Zotonic and have
Asterisk forward calls into Zotonic.

Installation
------------

The following configuration variables are needed:

 - `mod_asterisk.mgr_host` - The host that the Asterisk manager is
   listening on (defaults to `localhost`).
 - `mod_asterisk.mgr_port` - The port for the manager (defaults to 5038).
 - `mod_asterisk.mgr_name` - The username for the Asterisk manager connection (defaults to `asterisk`).
 - `mod_asterisk.mgr_secret` - The password for the manager (defaults to `asterisk`).
 - `mod_asterisk.agi_port` - The TCP port that mod_asterisk will
   listen on for incoming FastAGI connections from Asterisk. Defaults
   to 6666.

You need to configure your Asterisk's `extensions.conf` with a
statement to forward a call to the AGI port, like this:
`AGI(agi://127.0.0.1:6666)`.

Usage
-----

Once everything is running, you should be able to make a call and see
it mentioned in Zotonic's log.


    info     z_agi_handler:68     mod_asterisk: Incoming call but no handling module! 
    
This means that the call was picked up by the module but no Zotonic
module was in place to actually do something with the call. To
actually handle calls, you need to write your own module which
observes the `#asterisk_incoming{}` event:

    include_lib("modules/mod_asterisk/include/z_asterisk.hrl").
    
    observe_asterisk_incoming(#asterisk_incoming{agi_channel=Ch, environment=E}, Context) ->
        agi:answer(Ch),
        agi:say_digits(Ch, "12345", []),
        agi:hangup(Ch),
        ok.
        
Do not forget to return the `ok` atom, or the call will not be handled correctly.

Likewise, Asterisk Manager events are handled by observing a
notification on the `#asterisk_event{}` record.
