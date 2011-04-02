def c_file (path)
  result = [path]

  if File.readable?(path.ext('h').sub('/src', '/include'))
    result << path.ext('h').sub('/src', '/include')
  end

  result
end

def die (text)
  fail text
end
