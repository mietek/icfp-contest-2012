require File.expand_path('../board.rb', __FILE__)
require 'highline/system_extensions'
include HighLine::SystemExtensions

KEY_MAPPINGS = { 'w' => 'U', 'a' => 'L', 's' => 'D', 'd' => 'R', 'p' => 'A', 'o' => 'W' }
map_file = ARGV.first || 'contest1'

board = Board.new(File.open(File.expand_path("../../../tests/#{map_file}.map", __FILE__)))
board.draw

while true 
  key = KEY_MAPPINGS[get_character.chr]
  board.make_move(key)
  board.draw
end
