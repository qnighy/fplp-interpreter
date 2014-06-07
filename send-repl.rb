#!/usr/bin/ruby

ARGF.each_line do|line|
  $stderr.puts line
  $stderr.flush
  puts line
  $stdout.flush
  sleep 0.1
end
