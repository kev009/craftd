# Add in lexicographic order:
#
# pkginclude_HEADERS =
# ls craftd/*.h | awk '{ print $1" \\" }' | sort
# truncate last \
#
pkginclude_HEADERS = craftd/Arithmetic.h \
		     craftd/Buffer.h \
		     craftd/Buffers.h \
		     craftd/Client.h \
		     craftd/common.h \
		     craftd/Config.h \
		     craftd/Console.h \
		     craftd/Dynamic.h \
		     craftd/Error.h \
		     craftd/Event.h \
		     craftd/Hash.h \
		     craftd/HTTPd.h \
		     craftd/javaendian.h \
		     craftd/Job.h \
		     craftd/List.h \
		     craftd/Logger.h \
		     craftd/Map.h \
		     craftd/memory.h \
		     craftd/Plugin.h \
		     craftd/Plugins.h \
		     craftd/Protocol.h \
		     craftd/Regexp.h \
		     craftd/ScriptingEngine.h \
		     craftd/ScriptingEngines.h \
		     craftd/Server.h \
		     craftd/Set.h \
		     craftd/String.h \
		     craftd/TimeLoop.h \
		     craftd/utils.h \
		     craftd/version.h \
		     craftd/Worker.h \
		     craftd/Workers.h

# Modular protocol headers
survivaldir = $(pkgincludedir)/protocols/
survival_HEADERS = craftd/protocols/survival.h \
		    craftd/protocols/survival/Buffer.h \
		    craftd/protocols/survival/common.h \
		    craftd/protocols/survival/Logger.h \
		    craftd/protocols/survival/minecraft.h \
		    craftd/protocols/survival/Packet.h \
		    craftd/protocols/survival/PacketLength.h \
		    craftd/protocols/survival/Player.h \
		    craftd/protocols/survival/Region.h \
		    craftd/protocols/survival/World.h

# bstring headers
bstringdir = $(pkgincludedir)/bstring
bstring_HEADERS = craftd/bstring/bsafe.h \
		  craftd/bstring/bstraux.h \
		  craftd/bstring/bstrlib.h \
		  craftd/bstring/bstrwrap.h

# klib headers
klibdir = $(pkgincludedir)/klib
klib_HEADERS = craftd/klib/kbtree.h \
	       craftd/klib/khash.h \
	       craftd/klib/khmm.h \
	       craftd/klib/klist.h \
	       craftd/klib/kmin.h \
	       craftd/klib/knetfile.h \
	       craftd/klib/knhx.h \
	       craftd/klib/kseq.h \
	       craftd/klib/ksort.h \
	       craftd/klib/kstring.h \
	       craftd/klib/kvec.h
