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
		  extras.c \
		  Hash.c \
		  Job.c \
		  List.c \
		  Logger.c \
		  Map.c \
		  Plugin.c \
		  Plugins.c \
		  Protocol.c \
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

# Modular protocol dependant srcs
craftd_SOURCES += protocols/survival/Buffer.c \
		 protocols/survival/minecraft.c \
		 protocols/survival/Packet.c \
		 protocols/survival/PacketLength.c \
		 protocols/survival/Player.c \
		 protocols/survival/Region.c \
		 protocols/survival/World.c \
		 protocols/survival/main.c

craftd_LDFLAGS = -export-dynamic
craftd_LDADD = $(AM_LIBS) $(top_builddir)/third-party/libbstring.la

include $(top_srcdir)/build/auto/build.mk
