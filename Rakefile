require 'mkmf'
require 'rake'
require 'rake/clean'

load 'rake/variables.rb'
load 'rake/config.rb'
load 'rake/makefile.rb'

VERSION = '0.1a'

CC      = ENV['CC']  || 'gcc'
CXX     = ENV['CXX'] || 'g++'
CFLAGS  = "-Wall -Wextra -Wno-unused -std=gnu99 -fPIC #{ENV['CFLAGS']} -DCRAFTD_VERSION='\"#{VERSION}\"'"
LDFLAGS = "#{ENV['LDFLAGS']}"

if ENV['DEBUG']
  CFLAGS << ' -g3 -O0 -DCRAFTD_DEBUG'
else
  CFLAGS << ' -DNDEBUG -Os'
end

# Stuff building
task :default => ['craftd:build', 'craftd:plugins']

# Stuff installation
task :install => ['craftd:install']

# Static scripts generation
task :generate => [:clobber, 'generate:configure', 'generate:makefile']

namespace :craftd do |craftd|
  craftd.headers   = FileList['include/**/*.h']
  craftd.sources   = FileList['src/**/*.c', 'third-party/bstring/{bstrlib,bstraux}.c']
  craftd.libraries = '-lpthread -lz -ljansson -levent -levent_pthreads -lpcre -lltdl'

  CLEAN.include craftd.sources.ext('o')
  CLOBBER.include 'craftd', 'include/config.h'

  craftd.sources.each {|f|
    file f.ext('o') => f do
      sh "#{CC} #{CFLAGS} -Iinclude -o #{f.ext('o')} -c #{f}"
    end
  }

  desc 'Check for craftd requirements'
  task :requirements => 'include/config.h'
  
  file 'include/config.h' do
    have_library 'ltdl', 'lt_dlopen' or fail 'libtool not found'

    # check thread stuff
    have_library 'pthread' or fail 'pthread not found'
    have_func 'pthread_spin_init', 'pthread.h'

    # check for libevent 2
    have_library 'event' or fail 'libevent2 not found'
    have_library 'event_pthreads' or fail 'libevent2 with pthreads not found'
    have_macro '((_EVENT_NUMERIC_VERSION >> 24) == 2)', 'event2/event.h' do |c|
      c.sub('ifndef', 'if !')
    end

    # check for libjansson 2
    have_library 'jansson' or fail 'jansson not found'
    have_macro '(JANSSON_MAJOR_VERSION == 2)', 'jansson.h' do |c|
      c.sub('ifndef', 'if !')
    end

    have_library 'z' or fail 'zlib not found'
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

    create_config 'include/config.h'
  end

  file 'craftd' => craftd.sources.ext('o') do
    sh "#{CC} #{CFLAGS} #{craftd.sources.ext('o')} -o craftd -export-dynamic #{LDFLAGS} #{craftd.libraries}"
  end

  task :build => [:requirements, 'craftd']

  task :install => :build

  desc 'Build all plugins'
  task :plugins => ['plugin:protocol:build', 'plugin:persistence:build', 'plugin:commands:build']

  namespace :plugin do |plugin|
    plugin.names = ['protocol/beta', 'persistence/nbt', 'mapgen']

    class << plugin
      def file (name)
        "libcd#{name}.#{CONFIG['DLEXT']}"
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
        CLOBBER.include "plugins/#{plugin.file('protocol.beta')}", 'plugins/protocol/beta/include/config.h'

        beta.sources.each {|f|
          file f.ext('o') => f do
            sh "#{CC} #{CFLAGS} -Iinclude #{plugin.includes} -o #{f.ext('o')} -c #{f}"
          end
        }

        desc 'Check for beta plugin requirements'
        task :requirements => 'plugins/protocol/beta/include/config.h'

        file 'plugins/protocol/beta/include/config.h' do
          create_config 'plugins/protocol/beta/include/config.h'
        end

        file "plugins/#{plugin.file('protocol.beta')}" => beta.sources.ext('o') do
          sh "#{CC} #{CFLAGS} #{beta.sources.ext('o')} -shared -Wl,-soname,#{plugin.file('protocol.beta')} -o plugins/#{plugin.file('protocol.beta')} #{beta.libraries}"
        end

        desc 'Build beta plugin'
        task :build => [:requirements, "plugins/#{plugin.file('protocol.beta')}"]
      end
    end

    namespace :persistence do |persistence|
      task :build => ['nbt:build']

      namespace :nbt do |nbt|
        nbt.libraries = ''
        nbt.headers   = FileList['plugins/persistence/nbt/include/*.h']
        nbt.sources   = FileList['plugins/persistence/nbt/main.c', 'plugins/persistence/nbt/src/*.c',
          'plugins/persistence/nbt/cNBT/nbt_{parsing,treeops,util}.c']

        CLEAN.include nbt.sources.ext('o')
        CLOBBER.include "plugins/#{plugin.file('command.nbt')}", 'plugins/persistence/nbt/include/config.h'

        nbt.sources.each {|f|
          file f.ext('o') => f do
            sh "#{CC} #{CFLAGS} -Iinclude #{plugin.includes} -o #{f.ext('o')} -c #{f}"
          end
        }

        desc 'Check for nbt plugin requirements'
        task :requirements => 'plugins/persistence/nbt/include/config.h'

        file 'plugins/persistence/nbt/include/config.h' do
          create_config 'plugins/persistence/nbt/include/config.h'
        end

        file "plugins/#{plugin.file('persistence.nbt')}" => nbt.sources.ext('o') do
          sh "#{CC} #{CFLAGS} #{nbt.sources.ext('o')} -shared -Wl,-soname,#{plugin.file('persistence.nbt')} -o plugins/#{plugin.file('persistence.nbt')} #{nbt.libraries}"
        end

        desc 'Build nbt plugin'
        task :build => [:requirements, "plugins/#{plugin.file('persistence.nbt')}"]
      end
    end

    namespace :commands do |commands|
      task :build => []#['admin:build']

      namespace :admin do |admin|
        admin.libraries = ''
        admin.headers   = FileList['plugins/commands/admin/include/*.h']
        admin.sources   = FileList['plugins/commands/admin/main.c', 'plugins/commands/admin/src/*.c']

        CLEAN.include admin.sources.ext('o')
        CLOBBER.include "plugins/#{plugin.file('command.admin')}", 'plugins/admin/include/config.h'

        admin.sources.each {|f|
          file f.ext('o') => f do
            sh "#{CC} #{CFLAGS} -Iinclude #{plugin.includes} -o #{f.ext('o')} -c #{f}"
          end
        }

        file "plugins/#{plugin.file('command.admin')}" => admin.sources.ext('o') do
          sh "#{CC} #{CFLAGS} #{admin.sources.ext('o')} -shared -Wl,-soname,#{plugin.file('command.admin')} -o plugins/#{plugin.file('command.admin')} #{admin.libraries}"
        end

        desc 'Build admin plugin'
        task :build => [:requirements, "plugins/#{plugin.file('command.admin')}"]
      end
    end
  end
end
