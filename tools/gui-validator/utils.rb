def get_input
  if @options[:non_interactive]
    STDIN.getc
  else
    get_character.chr
  end
end

def run_cycle(key, interactive = true)
  cmd = (%w(U D R L W A).include? key) ? key : KEY_MAPPINGS[key]

  unless cmd.nil?
    @board.make_move(cmd)
    @board.draw if interactive
  else
    unless @options[:non_interactive]
      puts "Unknown input: #{key.inspect}"
      exit 1
    end
  end
end

