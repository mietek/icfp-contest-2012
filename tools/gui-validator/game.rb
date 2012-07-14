require File.expand_path('../board.rb', __FILE__)
require 'highline/system_extensions'
include HighLine::SystemExtensions

def run_cycle(cmd, key)
  unless cmd.nil?
    @board.make_move(cmd)
    @board.draw
  else
    puts 'Unknown input: #{key}'
    exit
  end
end

KEY_MAPPINGS = { 'w' => 'U', 'a' => 'L', 's' => 'D', 'd' => 'R', 'p' => 'A', 'o' => 'W' }
map_file = ARGV.first || 'contest1'
input = (ARGV[1] || '').split('')

@board = Board.new(File.open(File.expand_path("../../../tests/#{map_file}.map", __FILE__)))
@board.draw

while key = input.shift
  run_cycle(key, key)
end

while true 
  char = get_character.chr
  cmd = KEY_MAPPINGS[char]
  run_cycle(cmd, key)
end
