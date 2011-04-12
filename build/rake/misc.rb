def c_file (path)
  result = [path]

  if File.readable?(path.ext('h').sub('/src', '/include'))
    result << path.ext('h').sub('/src', '/include')
  end

  File.read(path).scan(/#\s*include\s*"(.*?\.c)"/).flatten.each {|f|
    result << "#{File.dirname(path)}/#{f}"
  } rescue nil

  result
end

def ldflags (libs=nil)
  ([libs] + global_libs).flatten.compact.map {|l|
    "-l#{l}"
  }.join(' ') + " #{LDFLAGS}"
end

CLOBBER.include '.global.libs'

def global_libs
  Marshal.load(File.read('.global.libs')) rescue []
end

def global_libs_add (value)
  old = global_libs

  File.open('.global.libs', 'w') {|f|
    f.write Marshal.dump((old + [value]).flatten)
  }

  value
end
