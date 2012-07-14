#!/usr/bin/env ruby 

require 'highline/system_extensions'
require 'optparse'
require File.expand_path('../board.rb', __FILE__)
require File.expand_path('../utils.rb', __FILE__)

include HighLine::SystemExtensions

KEY_MAPPINGS = { 'w' => 'U', 'a' => 'L', 's' => 'D', 'd' => 'R', 'p' => 'A', 'o' => 'W' }

@options = {}
optparse = OptionParser.new do |opts|
  opts.on('-h', '--help', 'Display this screen') do
    puts opts
    exit
  end
  opts.on('-vv', 'Run non-interactively') do |f|
    @options[:non_interactive] = true
  end
  opts.on('-i', '--input INPUT', 'Command line move input') do |input|
    @options[:input] = input.split('')
  end
end
optparse.parse!

map_file = ARGV.first || File.expand_path("../../../tests/contest1.map", __FILE__)

if @options[:non_interactive]
  @board = Board.new(File.open(map_file), false)
  while key = get_input
    run_cycle key, false
  end
  @board.formatted_draw unless @board.finished?
else
  @board = Board.new(File.open(map_file))
  @board.draw
  while key = (@options[:input] || []).shift
    run_cycle key
  end
  while true
    run_cycle get_input
  end
end
