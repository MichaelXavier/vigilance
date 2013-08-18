* config file for base set of watches. this is the common case i think

since name is unique, reloading the process should inject the configured
watches into the table's default acid state *and* update the applicable details
of the watches by name.

the only potentially weird part is that if you create a watch via the API and
then never add it to the config, it may be surprising when you find out its
still around. oh well.

* documentation of config stuff

Issues
* race condition with exit causes process to exit before logs flush. maybe a
  closable queue?
* waiting on fix in data-store for bug in S.map
* parallel test execution
* ID-oriented API is somewhat impractical. Probably a better idea to use names.
