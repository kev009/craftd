$gen_makefile  = false
$makefile_data = ''

CLEAN.include 'Makefile'

def sh (args)
  if $gen_makefile
    puts "Makefile: #{args}"
    $makefile_data << "\t#{args}\n"
  else
    super(args)
  end
end

desc 'Generate a makefile from the Rakefile'
task :makefile do
  $gen_makefile = true
  $makefile_data << "all: main\n"
  
  $makefile_data << "main:\n"
  Rake::Task[:default].invoke
  
  $makefile_data << "clean:\n"
  CLEAN.each do |f|
    $makefile_data << "\trm -rf #{f}\n"
  end
  
  $makefile_data << "clobber:\n"
  CLEAN.each do |f|
    $makefile_data << "\trm -rf #{f}\n"
  end

  CLOBBER.each do |f|
    $makefile_data << "\trm -rf #{f}\n"
  end
  
  File.open('Makefile', 'w') { |f| f.write $makefile_data}
end
