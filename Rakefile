require 'mkmf'
require 'rake'
require 'rake/clean'

load 'rake/variables.rb'
load 'rake/configure.rb'
load 'rake/misc.rb'

begin
  require 'rake/convert'
rescue LoadError
end

VERSION = '0.1a'

PREFIX         = (ENV['PREFIX']         ||= '/usr')
LIBDIR         = (ENV['LIBDIR']         ||= '${PREFIX}/lib')
LOCALESTATEDIR = (ENV['LOCALESTATEDIR'] ||= '/var')
DATADIR        = (ENV['DATADIR']        ||= '${PREFIX}/share')
SYSCONFDIR     = (ENV['SYSCONFDIR']     ||= '/etc')

if defined?(EXPORT)
  EXPORT << [:PREFIX, :LIBDIR, :LOCALESTATEDIR, :DATADIR, :SYSCONFDIR]
end

CC      = (ENV['CC'] ||= 'gcc')
CFLAGS  = "-Wall -Wno-unused -std=gnu99 -fPIC -DCRAFTD_VERSION='\"#{VERSION}\"'"
LDFLAGS = "-export-dynamic"

if ENV['DEBUG']
  CFLAGS << ' -g3 -O0 -DCRAFTD_DEBUG'
else
  CFLAGS << ' -DNDEBUG -Os'
end

# Stuff building
task :default => ['craftd:build', 'craftd:plugins']

# Stuff installation
task :install => ['craftd:install']

namespace :craftd do |craftd|
  craftd.headers   = FileList['include/**/*.h']
  craftd.sources   = FileList['src/**/*.c', 'third-party/bstring/{bstrlib,bstraux}.c']
  craftd.libraries = '-lpthread -lz -ljansson -levent -levent_pthreads -lpcre -lltdl'

  CLEAN.include craftd.sources.ext('o')
  CLOBBER.include 'craftd', 'include/config.h', 'craftd.conf.dist'

  craftd.sources.each {|f|
    file f.ext('o') => c_file(f) do
      sh "${CC} #{CFLAGS} ${CFLAGS} -Iinclude -o #{f.ext('o')} -c #{f}"
    end
  }

  desc 'Check for craftd requirements'
  task :requirements => 'include/config.h'
  
  file 'include/config.h' do
    have_library 'ltdl', 'lt_dlopen' or die 'libtool not found'

    # check thread stuff
    have_library 'pthread' or die 'pthread not found'
    have_func 'pthread_spin_init', 'pthread.h'

    # check for libevent 2
    have_library 'event' or die 'libevent2 not found'
    have_library 'event_pthreads' or die 'libevent2 with pthreads not found'
    have_macro '((_EVENT_NUMERIC_VERSION >> 24) == 2)', 'event2/event.h' do |c|
      c.sub('ifndef', 'if !')
    end

    # check for libjansson 2
    have_library 'jansson' or die 'jansson not found'
    have_macro '(JANSSON_MAJOR_VERSION == 2)', 'jansson.h' do |c|
      c.sub('ifndef', 'if !')
    end

    have_library 'z' or die 'zlib not found'
    have_library 'pcre' or die 'libpcre not found'

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

    create_config 'include/config.h'
  end

  file 'craftd' => craftd.sources.ext('o') do
    sh "${CC} #{CFLAGS} ${CFLAGS} #{craftd.sources.ext('o')} -o craftd #{craftd.libraries} #{LDFLAGS} ${LDFLAGS}"
  end

  file 'craftd.conf.dist' => 'craftd.conf.dist.in' do
    sh %{rm -f craftd.conf.dist craftd.conf.dist.tmp}
    sh %{srcdir=''}
    sh %{test -f ./craftd.conf.dist.in || srcdir=./;}
    sh %{sed -e 's|@localstatedir[@]|${LOCALESTATEDIR}|g' -e 's|@datadir[@]|${DATADIR}|g' -e 's|@sysconfdir[@]|${SYSCONFDIR}|g' -e 's|@libdir[@]|${LIBDIR}|g' craftd.conf.dist.in > craftd.conf.dist.tmp}
    sh %{mv craftd.conf.dist.tmp craftd.conf.dist}
  end

  task :build => [:requirements, 'craftd', 'craftd.conf.dist']

  task :install => :build

  desc 'Build all plugins'
  task :plugins => ['plugin:protocol:build', 'plugin:persistence:build', 'plugin:mapgen:build',
    'plugin:commands:build', 'plugin:tests:build']

  namespace :plugin do |plugin|
    plugin.names = ['protocol/beta', 'persistence/nbt', 'mapgen']

    class << plugin
      def file (name)
        "#{name}.#{CONFIG['DLEXT']}"
      end

      def includes
        self.names.map {|p|
          "-Iplugins/#{p}/include"
        }.join(' ')
      end
    end

    namespace :protocol do |protocol|
      task :build => ['beta:build']

      namespace :beta do |beta|
        beta.libraries = ''
        beta.headers   = FileList['plugins/protocol/beta/include/*.h']
        beta.sources   = FileList['plugins/protocol/beta/**/*.c'].exclude('callbacks.c')

        CLEAN.include beta.sources.ext('o')
        CLOBBER.include "plugins/#{plugin.file('protocol.beta')}"

        beta.sources.each {|f|
          if f.end_with?('main.c')
            file f.ext('o') => [f, "#{File.dirname(f)}/callbacks.c"] do
              sh "${CC} #{CFLAGS} ${CFLAGS} -Iinclude #{plugin.includes} -o #{f.ext('o')} -c #{f}"
            end
          else
            file f.ext('o') => c_file(f) do
              sh "${CC} #{CFLAGS} ${CFLAGS} -Iinclude #{plugin.includes} -o #{f.ext('o')} -c #{f}"
            end
          end
        }

        file "plugins/#{plugin.file('protocol.beta')}" => beta.sources.ext('o') do
          sh "${CC} #{CFLAGS} ${CFLAGS} #{beta.sources.ext('o')} -shared -Wl,-soname,#{plugin.file('protocol.beta')} -o plugins/#{plugin.file('protocol.beta')} #{beta.libraries} #{LDFLAGS} ${LDFLAGS}"
        end

        desc 'Build beta plugin'
        task :build => ["plugins/#{plugin.file('protocol.beta')}"]
      end
    end

    namespace :persistence do |persistence|
      task :build => ['nbt:build']

      namespace :nbt do |nbt|
        nbt.headers = FileList['plugins/persistence/nbt/include/*.h']
        nbt.sources = FileList['plugins/persistence/nbt/main.c', 'plugins/persistence/nbt/src/*.c',
          'plugins/persistence/nbt/cNBT/nbt_{loading,parsing,treeops,util}.c']

        CLEAN.include nbt.sources.ext('o')
        CLOBBER.include "plugins/#{plugin.file('persistence.nbt')}"

        nbt.sources.each {|f|
          file f.ext('o') => c_file(f) do
            sh "${CC} #{CFLAGS} ${CFLAGS} -Iinclude #{plugin.includes} -Iplugins/persistence/nbt -o #{f.ext('o')} -c #{f}"
          end
        }

        file "plugins/#{plugin.file('persistence.nbt')}" => nbt.sources.ext('o') do
          sh "${CC} #{CFLAGS} ${CFLAGS} #{nbt.sources.ext('o')} -shared -Wl,-soname,#{plugin.file('persistence.nbt')} -o plugins/#{plugin.file('persistence.nbt')} #{nbt.libraries} #{LDFLAGS} ${LDFLAGS}"
        end

        desc 'Build nbt plugin'
        task :build => ["plugins/#{plugin.file('persistence.nbt')}"]
      end
    end

    namespace :mapgen do |mapgen|
      task :build => ['classic:build', 'trivial:build']

      namespace :classic do |classic|
        classic.sources = FileList['plugins/mapgen/classic/main.c', 'plugins/mapgen/noise/simplexnoise1234.c']

        CLEAN.include classic.sources.ext('o')
        CLOBBER.include "plugins/#{plugin.file('mapgen.classic')}"

        classic.sources.each {|f|
          if f.end_with?('main.c')
            file f.ext('o') => [f, "#{File.dirname(f)}/helpers.c"] do
              sh "${CC} #{CFLAGS} ${CFLAGS} -Iinclude #{plugin.includes} -Iplugins/mapgen -o #{f.ext('o')} -c #{f}"
            end
          else
            file f.ext('o') => c_file(f) do
              sh "${CC} #{CFLAGS} ${CFLAGS} -Iinclude #{plugin.includes} -Iplugins/mapgen -o #{f.ext('o')} -c #{f}"
            end
          end

        }

        file "plugins/#{plugin.file('mapgen.classic')}" => classic.sources.ext('o') do
          sh "${CC} #{CFLAGS} ${CFLAGS} #{classic.sources.ext('o')} -shared -Wl,-soname,#{plugin.file('persistence.nbt')} -o plugins/#{plugin.file('mapgen.classic')} #{LDFLAGS} ${LDFLAGS}"
        end

        desc 'Build classic mapgen'
        task :build => "plugins/#{plugin.file('mapgen.classic')}"
      end

      namespace :trivial do |trivial|
        trivial.sources = FileList['plugins/mapgen/trivial/main.c']

        CLEAN.include trivial.sources.ext('o')
        CLOBBER.include "plugins/#{plugin.file('mapgen.trivial')}"

        trivial.sources.each {|f|
          file f.ext('o') => c_file(f) do
            sh "${CC} #{CFLAGS} ${CFLAGS} -Iinclude #{plugin.includes} -o #{f.ext('o')} -c #{f}"
          end
        }

        file "plugins/#{plugin.file('mapgen.trivial')}" => trivial.sources.ext('o') do
          sh "${CC} #{CFLAGS} ${CFLAGS} #{trivial.sources.ext('o')} -shared -Wl,-soname,#{plugin.file('persistence.nbt')} -o plugins/#{plugin.file('mapgen.trivial')} #{LDFLAGS} ${LDFLAGS}"
        end

        desc 'Build trivial mapgen'
        task :build => "plugins/#{plugin.file('mapgen.trivial')}"
      end
    end

    namespace :commands do |commands|
      task :build => ['admin:build']

      namespace :admin do |admin|
        admin.sources = FileList['plugins/commands/admin/main.c']

        CLEAN.include admin.sources.ext('o')
        CLOBBER.include "plugins/#{plugin.file('commands.admin')}"

        admin.sources.each {|f|
          file f.ext('o') => c_file(f) do
            sh "${CC} #{CFLAGS} ${CFLAGS} -Iinclude #{plugin.includes} -o #{f.ext('o')} -c #{f} ${LDFLAGS}"
          end
        }

        file "plugins/#{plugin.file('commands.admin')}" => admin.sources.ext('o') do
          sh "${CC} #{CFLAGS} ${CFLAGS} #{admin.sources.ext('o')} -shared -Wl,-soname,#{plugin.file('commands.admin')} -o plugins/#{plugin.file('commands.admin')} #{admin.libraries} ${LDFLAGS}"
        end

        desc 'Build admin plugin'
        task :build => [:requirements, "plugins/#{plugin.file('commands.admin')}"]
      end
    end

    namespace :tests do |tests|
      tests.sources = FileList['plugins/tests/main.c', 'plugins/tests/tinytest/tinytest.c']

      CLEAN.include tests.sources.ext('o')
      CLOBBER.include "plugins/#{plugin.file('tests')}"

      tests.sources.each {|f|
        file f.ext('o') => c_file(f) do
          sh "${CC} #{CFLAGS} -Wno-extra ${CFLAGS} -Iinclude -Iplugins/tests #{plugin.includes} -o #{f.ext('o')} -c #{f}"
        end
      }

      file "plugins/#{plugin.file('tests')}" => tests.sources.ext('o') do
        sh "${CC} #{CFLAGS} ${CFLAGS} #{tests.sources.ext('o')} -shared -Wl,-soname,#{plugin.file('tests')} -o plugins/#{plugin.file('tests')} #{LDFLAGS} ${LDFLAGS}"
      end

      desc 'Build tests plugin'
      task :build => "plugins/#{plugin.file('tests')}"
    end
  end
end
