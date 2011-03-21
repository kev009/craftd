$generate = false
$makefile = ''

CLEAN.include 'Makefile'

def sh (args)
  if $generate
    puts "Makefile: #{args}"
    $makefile << "\t#{args}\n"
  else
    super(args)
  end
end

namespace :generate do |generate|
  desc 'Generate a Makefile from the Rakefile'
  task :makefile do
    $generate = true
    $makefile << "all: main\n"
    
    $makefile << "main:\n"
    Rake::Task[:default].invoke

    $makefile << "install:\n"
    Rake::Task[:install].invoke
    
    $makefile << "clean:\n"
    CLEAN.each do |f|
      $makefile << "\trm -rf #{f}\n"
    end
    
    $makefile << "clobber:\n"
    CLEAN.each do |f|
      $makefile << "\trm -rf #{f}\n"
    end

    CLOBBER.each do |f|
      $makefile << "\trm -rf #{f}\n"
    end
    
    File.open('Makefile', 'w') {|f|
      f.write $makefile
    }

    $makefile = ''
  end
end
