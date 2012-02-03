#!/usr/bin/env ruby

File.open("setup","r").each_line do |line|
  if line =~ /^linkup/i
    path = line.split[1]
    file = path.split("/")[1]
    puts file
    `mv #{file} #{file}.linkme`
  end
end
