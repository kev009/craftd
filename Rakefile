require 'mkmf'
require 'rake'
require 'rake/clean'

FileList['build/rake/**.rb'].each do |f| load f end

VERSION = '0.2pre'

PREFIX         = ENV['PREFIX']         || '/usr'
LIBDIR         = ENV['LIBDIR']         || "#{PREFIX}/lib"
LOCALESTATEDIR = ENV['LOCALESTATEDIR'] || '/var'
DATADIR        = ENV['DATADIR']        || "#{PREFIX}/share"
SYSCONFDIR     = ENV['SYSCONFDIR']     || '/etc'

$CFLAGS   << ' ${CFLAGS}'
$INCFLAGS << ' ${CFLAGS}'
$LDFLAGS  << ' ${LDFLAGS}'

CC      = ENV['CC'] || 'gcc'
CFLAGS  = "-Wall -Wno-unused -std=gnu99 -fPIC -DCRAFTD_VERSION='\"#{VERSION}\"' -DPACKAGE_STRING='\"craftd #{VERSION}\"' #{ENV['CFLAGS']}"
LDFLAGS = "-export-dynamic #{ENV['LDFLAGS']}"

if ENV['DEBUG']
  CFLAGS << ' -g3 -O0 -DCRAFTD_DEBUG'
else
  CFLAGS << ' -DNDEBUG -Os'
end

# Stuff building
task :default => ['craftd:build', 'plugins:build']

task :all => ['craftd:build', 'plugins:build', 'plugins:rpc:build', 'scripting:build']

# Stuff installation
task :install => ['craftd:install']

namespace :craftd do |craftd|
  craftd.headers   = FileList['include/**/*.h']
  craftd.sources   = FileList['src/**/*.c', 'third-party/bstring/{bstrlib,bstraux}.c']
  craftd.libraries = %w(pthread z event event_pthreads pcre ltdl)

  CLEAN.include craftd.sources.ext('o')
  CLOBBER.include 'craftd', 'include/craftd/config.h', 'craftd.conf.dist'

  craftd.sources.each {|f|
    file f.ext('o') => c_file(f) do
      sh "#{CC} #{CFLAGS} -Iinclude -o #{f.ext('o')} -c #{f}"
    end
  }

  desc 'Check for craftd requirements'
  task :requirements => 'include/craftd/config.h'
  
  file 'include/craftd/config.h' do
    have_library 'ltdl', 'lt_dlopen' or fail 'libtool not found'

    # check thread stuff
    have_header 'pthread.h' or fail 'pthread-dev not found'
    have_library 'pthread' or fail 'pthread not found'
    have_func 'pthread_spin_init', 'pthread.h'

    # check for libevent 2
    have_library 'event' or fail 'libevent2 not found'
    have_library 'event_pthreads' or fail 'libevent2 with pthreads not found'
    have_macro '((_EVENT_NUMERIC_VERSION >> 24) == 2)', 'event2/event.h' do |c|
      c.sub('ifndef', 'if !')
    end

    # zlib checks
    have_header 'zlib.h' or fail 'zlib-dev not found'
    have_library 'z' or fail 'zlib not found'

    # pcre checks
    have_header 'pcre.h' or fail 'libpcre-dev not found'
    have_library 'pcre' or fail 'libpcre not found'

    # endianness stuff
    have_header 'endian.h'
    have_header 'sys/endian.h'
    have_header 'netinet/in.h'
    have_header 'arpa/inet.h'

    have_func 'be64toh'
    have_func 'htobe64'

    check_sizeof 'FUNCTION_POINTER' do |c|
       "typedef void(*FUNCTION_POINTER)();\n" + c
    end

    check_sizeof 'POINTER' do |c|
       "typedef void* POINTER;\n" + c
    end

    # check for libjansson2 for RPC.JSON
    if have_header 'jansson.h' and have_library 'jansson' and have_macro '(JANSSON_MAJOR_VERSION == 2)', 'jansson.h' do |c| c.sub('ifndef', 'if !') end
      craftd.libraries << 'jansson'

      $defs << '-DHAVE_JSON'
    end

    # check for libxml2 for RPC.XML
    if have_library 'xml2'
      craftd.libraries << 'xml2'

      $defs << '-DHAVE_XML'
    end

    create_config 'include/craftd/config.h'
  end

  file 'craftd' => craftd.sources.ext('o') do
    sh "#{CC} #{CFLAGS} #{craftd.sources.ext('o')} -o craftd #{craftd.libraries.map { |l| "-l#{l}" }.join(' ')} #{LDFLAGS}"
  end

  file 'craftd.conf.dist' => 'craftd.conf.dist.in' do
    sh %{rm -f craftd.conf.dist craftd.conf.dist.tmp}
    sh %{srcdir=''}
    sh %{test -f ./craftd.conf.dist.in || srcdir=./;}
    sh %{sed \
      -e "s|@localstatedir[@]|#{Regexp.escape(LOCALESTATEDIR)}|g" \
      -e "s|@datadir[@]|#{Regexp.escape(DATADIR)}|g" \
      -e "s|@sysconfdir[@]|#{Regexp.escape(SYSCONFDIR)}|g" \
      -e "s|@libdir[@]|#{Regexp.escape(LIBDIR)}|g" \
      craftd.conf.dist.in > craftd.conf.dist.tmp}
    sh %{mv craftd.conf.dist.tmp craftd.conf.dist}
  end

  task :build => [:requirements, 'craftd', 'craftd.conf.dist']

  task :install => :build
end

namespace :plugins do |plugin|
  desc 'Build all plugins'
  task :build => ['survival:build']

  class << plugin
    def file (name)
      "#{name}.#{CONFIG['DLEXT']}"
    end
  end

  namespace :survival do |plugin|
    class << plugin
      def file (name)
        "survival.#{name}.#{CONFIG['DLEXT']}"
      end
    end

    task :build => ['base:build', 'persistence:build', 'mapgen:build', 'commands:build', 'tests:build']

    namespace :base do |base|
      base.sources = FileList['plugins/survival/base/main.c']

      CLEAN.include base.sources.ext('o')
      CLOBBER.include "plugins/#{plugin.file('base')}"

      base.sources.each {|f|
        file f.ext('o') => c_file(f) do
          sh "#{CC} #{CFLAGS} -Iinclude -o #{f.ext('o')} -c #{f}"
        end
      }

      file "plugins/#{plugin.file('base')}" => base.sources.ext('o') do
        sh "#{CC} #{CFLAGS} #{base.sources.ext('o')} -shared -Wl,-soname,#{plugin.file('base')} -o plugins/#{plugin.file('base')} #{LDFLAGS}"
      end

      desc 'Build SMP base plugin'
      task :build => "plugins/#{plugin.file('base')}"
    end

    namespace :persistence do |persistence|
      task :build => ['nbt:build']

      namespace :nbt do |nbt|
        nbt.cflags = '-Iplugins/survival/persistence/nbt -Iplugins/survival/persistence/nbt/include'

        nbt.sources = FileList[
          'plugins/survival/persistence/nbt/main.c',
          'plugins/survival/persistence/nbt/src/*.c',
          'plugins/survival/persistence/nbt/cNBT/nbt_{loading,parsing,treeops,util}.c',
          'plugins/survival/persistence/nbt/cNBT/{buffer}.c']

        CLEAN.include nbt.sources.ext('o')
        CLOBBER.include "plugins/#{plugin.file('persistence.nbt')}"

        nbt.sources.each {|f|
          file f.ext('o') => c_file(f) do
            sh "#{CC} #{CFLAGS} -Iinclude #{nbt.cflags} -o #{f.ext('o')} -c #{f}"
          end
        }

        file "plugins/#{plugin.file('persistence.nbt')}" => nbt.sources.ext('o') do
          sh "#{CC} #{CFLAGS} #{nbt.sources.ext('o')} -shared -Wl,-soname,#{plugin.file('persistence.nbt')} -o plugins/#{plugin.file('persistence.nbt')} #{LDFLAGS}"
        end

        desc 'Build nbt plugin'
        task :build => "plugins/#{plugin.file('persistence.nbt')}"
      end
    end

    namespace :mapgen do |mapgen|
      task :build => ['classic:build', 'trivial:build']

      namespace :classic do |classic|
        classic.cflags  = '-Iplugins/survival/mapgen'
        classic.ldflags = '-lm'

        classic.sources = FileList['plugins/survival/mapgen/classic/main.c', 'plugins/survival/mapgen/noise/simplexnoise1234.c']

        CLEAN.include classic.sources.ext('o')
        CLOBBER.include "plugins/#{plugin.file('mapgen.classic')}"

        classic.sources.each {|f|
          file f.ext('o') => c_file(f) do
            sh "#{CC} #{CFLAGS} -Iinclude #{classic.cflags} -o #{f.ext('o')} -c #{f}"
          end
        }

        file "plugins/#{plugin.file('mapgen.classic')}" => classic.sources.ext('o') do
          sh "#{CC} #{CFLAGS} #{classic.sources.ext('o')} -shared -Wl,-soname,#{plugin.file('persistence.nbt')} -o plugins/#{plugin.file('mapgen.classic')} #{classic.ldflags} #{LDFLAGS}"
        end

        desc 'Build classic mapgen'
        task :build => "plugins/#{plugin.file('mapgen.classic')}"
      end

      namespace :trivial do |trivial|
        trivial.sources = FileList['plugins/survival/mapgen/trivial/main.c']

        CLEAN.include trivial.sources.ext('o')
        CLOBBER.include "plugins/#{plugin.file('mapgen.trivial')}"

        trivial.sources.each {|f|
          file f.ext('o') => c_file(f) do
            sh "#{CC} #{CFLAGS} -Iinclude -o #{f.ext('o')} -c #{f}"
          end
        }

        file "plugins/#{plugin.file('mapgen.trivial')}" => trivial.sources.ext('o') do
          sh "#{CC} #{CFLAGS} #{trivial.sources.ext('o')} -shared -Wl,-soname,#{plugin.file('persistence.nbt')} -o plugins/#{plugin.file('mapgen.trivial')} #{LDFLAGS}"
        end

        desc 'Build trivial mapgen'
        task :build => "plugins/#{plugin.file('mapgen.trivial')}"
      end
    end

    namespace :commands do |commands|
      task :build => []#'admin:build']

      namespace :admin do |admin|
        admin.sources = FileList['plugins/commands/admin/main.c']

        CLEAN.include admin.sources.ext('o')
        CLOBBER.include "plugins/#{plugin.file('commands.admin')}"

        admin.sources.each {|f|
          file f.ext('o') => c_file(f) do
            sh "#{CC} #{CFLAGS} -Iinclude -o #{f.ext('o')} -c #{f}"
          end
        }

        file "plugins/#{plugin.file('commands.admin')}" => admin.sources.ext('o') do
          sh "#{CC} #{CFLAGS} #{admin.sources.ext('o')} -shared -Wl,-soname,#{plugin.file('commands.admin')} -o plugins/#{plugin.file('commands.admin')}"
        end

        desc 'Build admin plugin'
        task :build => "plugins/#{plugin.file('commands.admin')}"
      end
    end

    namespace :tests do |tests|
      tests.cflags = '-Iplugins/survival/tests'

      tests.sources = FileList['plugins/survival/tests/main.c', 'plugins/survival/tests/tinytest/tinytest.c']

      CLEAN.include tests.sources.ext('o')
      CLOBBER.include "plugins/#{plugin.file('tests')}"

      tests.sources.each {|f|
        file f.ext('o') => c_file(f) do
          sh "#{CC} #{CFLAGS} -Iinclude #{tests.cflags} -o #{f.ext('o')} -c #{f}"
        end
      }

      file "plugins/#{plugin.file('tests')}" => tests.sources.ext('o') do
        sh "#{CC} #{CFLAGS} #{tests.sources.ext('o')} -shared -Wl,-soname,#{plugin.file('tests')} -o plugins/#{plugin.file('tests')} #{LDFLAGS}"
      end

      desc 'Build tests plugin'
      task :build => "plugins/#{plugin.file('tests')}"
    end
  end

  namespace :rpc do |rpc|
    rpc.sources   = FileList['plugins/rpc/main.c']
    rpc.libraries = []

    CLEAN.include rpc.sources.ext('o')
    CLOBBER.include "plugins/#{plugin.file('rpc')}"

    rpc.sources.each {|f|
      file f.ext('o') => c_file(f) do
        sh "#{CC} #{CFLAGS} -Iinclude -o #{f.ext('o')} -c #{f}"
      end
    }

    file "plugins/#{plugin.file('rpc')}" => rpc.sources.ext('o') do
      sh "#{CC} #{CFLAGS} #{rpc.sources.ext('o')} -shared -Wl,-soname,#{plugin.file('rpc')} -o plugins/#{plugin.file('rpc')} #{rpc.libraries.map { |l| "-l#{l}" }.join(' ')} #{LDFLAGS}"
    end

    desc 'check for RPC daemon requirements'
    task :requirements => 'plugins/rpc/config.h'

    file 'plugins/rpc/config.h' do
      # check for libjansson2 for RPC.JSON
      if have_header 'jansson.h' and have_library 'jansson' and have_macro '(JANSSON_MAJOR_VERSION == 2)', 'jansson.h' do |c| c.sub('ifndef', 'if !') end
        rpc.libraries << 'jansson'

        $defs << '-DHAVE_JSON'
      end

      # check for libxml2 for RPC.XML
      if have_library 'xml2'
        rpc.libraries << 'xml2'

        $defs << '-DHAVE_XML'
      end

      create_config 'plugins/rpc/config.h'
    end

    desc 'Build RPC daemon'
    task :build => [:requirements, "plugins/#{plugin.file('rpc')}"]
  end
end

namespace :scripting do |scripting|
  desc 'Build all scripting support'
  task :build => ['craftd:build', 'lisp:build', 'javascript:build']

  class << scripting
    def file (name)
      "#{name}.#{CONFIG['DLEXT']}"
    end
  end

  namespace :lisp do |lisp|
    lisp.sources = FileList['scripting/lisp/main.c']

    CLEAN.include lisp.sources.ext('o')
    CLOBBER.include "scripting/#{scripting.file('lisp')}", 'scripting/lisp/include/config.h'

    lisp.sources.each {|f|
      file f.ext('o') => c_file(f) do
        sh "#{CC} #{CFLAGS} $(ecl-config --cflags) -Iinclude -o #{f.ext('o')} -c #{f}"
      end
    }

    file "scripting/#{scripting.file('lisp')}" => lisp.sources.ext('o') do
      sh "#{CC} #{CFLAGS} #{lisp.sources.ext('o')} -shared -Wl,-soname,#{scripting.file('lisp')} -o scripting/#{scripting.file('lisp')} $(ecl-config --ldflags)"
    end

    desc 'Check for Common LISP requirements'
    task :requirements => 'scripting/lisp/include/config.h'

    file 'scripting/lisp/include/config.h' do
      find_executable('ecl-config') or fail 'ecl-config not found'

      create_config 'scripting/lisp/include/config.h'
    end

    desc 'Build Common LISP scripting'
    task :build => [:requirements, "scripting/#{scripting.file('lisp')}"]
  end

  namespace :javascript do |javascript|
    javascript.cflags  = '-DXP_UNIX'
    javascript.sources = FileList['scripting/javascript/main.c', 'scripting/javascript/src/**.c']

    CLEAN.include javascript.sources.ext('o')
    CLOBBER.include "scripting/#{scripting.file('javascript')}", 'scripting/javascript/include/config.h'

    javascript.sources.each {|f|
      file f.ext('o') => c_file(f) do
        sh "#{CC} #{CFLAGS} #{javascript.cflags} $(js-config --cflags) -Iinclude -o #{f.ext('o')} -c #{f}"
      end
    }

    file "scripting/#{scripting.file('javascript')}" => javascript.sources.ext('o') do
      sh "#{CC} #{CFLAGS} #{javascript.sources.ext('o')} -shared -Wl,-soname,#{scripting.file('javascript')} -o scripting/#{scripting.file('javascript')} $(js-config --libs)"
    end

    desc 'Check for JavaScript requirements'
    task :requirements => 'scripting/javascript/include/config.h'

    file 'scripting/javascript/include/config.h' do
      find_executable('js-config') or fail 'js-config not found'

      have_const 'JS_HAS_CTYPES', 'js-config.h', `js-config --cflags`.strip

      create_config 'scripting/javascript/include/config.h'
    end

    desc 'Build JavaScript scripting'
    task :build => [:requirements, "scripting/#{scripting.file('javascript')}"]
  end
end

