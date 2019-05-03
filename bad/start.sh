
#if you want to use a different port, then start like this:
# sh start 3666

#sh clean.sh #this deletes the database so every time we re-start, we have 0 blocks again. only needed during testing.
./rebar get-deps
sh clean.sh #this deletes the database so every time we re-start, we have 0 blocks again. only needed during testing.
./rebar compile #this line checks if any modules were modified, and recompiles them if they were. only needed during testing.
mkdir data
erl -pa ebin deps/*/ebin/ -eval "application:ensure_all_started(amoveo_p2p_derivatives_explorer)"
