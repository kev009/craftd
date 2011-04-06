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
