require 'mkmf'
require 'rake'
require 'rake/clean'

load 'rake/variables.rb'
load 'rake/config.rb'
load 'rake/makefile.rb'

VERSION = '0.1a'

CC      = ENV['CC'] || 'gcc'
CFLAGS  = "-Wall -Wextra -Wno-unused -std=gnu99 #{ENV['CFLAGS'] || $CFLAGS} -DCRAFTD_VERSION='\"#{VERSION}\"'"
LDFLAGS = "#{ENV['LDFLAGS'] || $LDFLAGS}"

if ENV['DEBUG']
  CFLAGS << ' -g3 -O0 -DCRAFTD_DEBUG'
else
  CFLAGS << ' -DNDEBUG'
end

task :default => ['craftd:build', 'craftd:plugins']

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

  task :install => :build do

  end

  desc 'Build all plugins'
  task :plugins => ['plugin:beta:build', 'plugin:nbt:build']

  namespace :plugin do |plugin|
    plugin.names = [:beta, :nbt, :mapgen, :tests]

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

    namespace :beta do |beta|
      beta.headers   = FileList['plugins/beta/include/*.h']
      beta.sources   = FileList['plugins/beta/**/*.c'].exclude('callbacks.c')
      beta.libraries = ''

      CLEAN.include beta.sources.ext('o')
      CLOBBER.include "plugins/#{plugin.file(:beta)}", 'plugins/beta/include/config.h'

      beta.sources.each {|f|
        file f.ext('o') => f do
          sh "#{CC} #{CFLAGS} -Iinclude #{plugin.includes} -o #{f.ext('o')} -c #{f}"
        end
      }

      desc 'Check for beta plugin requirements'
      task :requirements => 'plugins/beta/include/config.h'

      file 'plugins/beta/include/config.h' do
        create_config 'plugins/beta/include/config.h'
      end

      file "plugins/#{plugin.file(:beta)}" => beta.sources.ext('o') do
        sh "#{CC} #{CFLAGS} #{beta.sources.ext('o')} -shared -Wl,-soname,#{plugin.file(:beta)} -o plugins/#{plugin.file(:beta)} #{beta.libraries}"
      end

      desc 'Build beta plugin'
      task :build => [:requirements, "plugins/#{plugin.file(:beta)}"]
    end

    namespace :nbt do |nbt|
      nbt.headers = FileList['plugins/nbt/include/*.h']
      nbt.sources = FileList['plugins/nbt/main.c', 'plugins/nbt/src/*.c',
        'plugins/nbt/cNBT/nbt_{parsing,treeops,util}.c']

      nbt.libraries = ''

      CLEAN.include nbt.sources.ext('o')
      CLOBBER.include "plugins/#{plugin.file(:nbt)}", 'plugins/nbt/include/config.h'

      nbt.sources.each {|f|
        file f.ext('o') => f do
          sh "#{CC} #{CFLAGS} -Iinclude #{plugin.includes} -o #{f.ext('o')} -c #{f}"
        end
      }

      desc 'Check for nbt plugin requirements'
      task :requirements => 'plugins/nbt/include/config.h'

      file 'plugins/nbt/include/config.h' do
        create_config 'plugins/nbt/include/config.h'
      end

      file "plugins/#{plugin.file(:nbt)}" => nbt.sources.ext('o') do
        sh "#{CC} #{CFLAGS} #{nbt.sources.ext('o')} -shared -Wl,-soname,#{plugin.file(:nbt)} -o plugins/#{plugin.file(:nbt)} #{nbt.libraries}"
      end

      desc 'Build nbt plugin'
      task :build => [:requirements, "plugins/#{plugin.file(:nbt)}"]
    end

    namespace :admin do |admin|
      admin.headers   = FileList['plugins/admin/include/*.h']
      admin.sources   = FileList['plugins/admin/main.c', 'plugins/admin/src/*.c']
      admin.libraries = ''

      CLEAN.include admin.sources.ext('o')
      CLOBBER.include "plugin/#{plugin.file(:admin)}", 'plugins/admin/include/config.h'

      admin.sources.each {|f|
        file f.ext('o') => f do
          sh "#{CC} #{CFLAGS} -Iinclude #{includes} -o #{f.ext('o')} -c #{f}"
        end
      }

      file "plugin/#{plugin.file(:admin)}" => admin.sources.ext('o') do
        sh "#{CC} #{CFLAGS} #{admin.sources.ext('o')} -shared -Wl,-soname,#{plugin.file(:admin)} -o plugins/#{plugin.file(:admin)} #{admin.libraries}"
      end

      desc 'Build admin plugin'
      task :build => [:requirements, "plugins/#{plugin.file(:admin)}"]
    end
  end
end
