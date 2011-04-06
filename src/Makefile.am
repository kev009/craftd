bin_PROGRAMS = craftd

# Add in lexicographic order:
#
# craftd_SOURCES += 
# ls *.c | awk '{ print $1" \\" }' | sort
# truncate last \
#
craftd_SOURCES =  Buffer.c \
		  Buffers.c \
		  Client.c \
		  Config.c \
		  Console.c \
		  ConsoleLogger.c \
		  craftd.c \
		  Dynamic.c \
		  Error.c \
		  Event.c \
		  Hash.c \
		  HTTPd.c \
		  Job.c \
		  List.c \
		  Logger.c \
		  Map.c \
		  Plugin.c \
		  Plugins.c \
		  Regexp.c \
		  ScriptingEngine.c \
		  ScriptingEngines.c \
		  Server.c \
		  Set.c \
		  String.c \
		  SystemLogger.c \
		  TimeLoop.c \
		  utils.c \
		  Worker.c \
		  Workers.c

craftd_SOURCES += $(bstringdir)/bstrlib.c $(bstringdir)/bstrlib.h
craftd_SOURCES += $(bstringdir)/bstraux.c $(bstringdir)/bstraux.h

craftd_LDFLAGS = $(libevent2_pthreads_LIBS) $(jansson_LIBS) -export-dynamic
craftd_LDADD = $(PTHREAD_LIBS)

include $(top_srcdir)/build/auto/build.mk
