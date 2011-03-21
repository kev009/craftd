$generate  = false
$configure = ''

UNIVERSAL_INTS.clear if defined?(UNIVERSAL_INTS)

def create_config (path)
  create_header path

  $defs.clear
end

namespace :generate do |generate|
  desc 'Generate a Makefile from the Rakefile'
  task :configure do

  end
end
