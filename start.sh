#set up a config file if it does not already exist.
if [ ! -f config/sys.config ]; then
cp config/sys.config.tmpl config/sys.config
    echo "File not found!"
fi

# First recompile the code and rebuild the release.
./rebar3 compile
./rebar3 as prod release
# then launch the software
./_build/prod/rel/amoveo_p2p_derivatives_explorer/bin/amoveo_p2p_derivatives_explorer start
