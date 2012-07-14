class Board
  attr_reader :data, :moves_log, :score

  def initialize(file, interactive = true)
    @moves_log   = []
    @score       = 0
    @lambdas     = 0
    @interactive = interactive
    parse_board(file)
    @next_flood = @flooding if @flooding > 0
    @steps_underwater = 0
  end

  def make_move(direction)
    @moves_log << direction
    @score -= 1

    current_x, current_y = current_position
    case direction
    when 'U' then
      move_to(current_y + 1, current_x)
    when 'D' then
      move_to(current_y - 1, current_x)
    when 'R' then
      move_to(current_y, current_x + 1)
    when 'L' then
      move_to(current_y, current_x - 1)
    when 'W' then
      ;
    when 'A' then
      abort!
    end
    tick
  end

  def draw
    40.times { puts '' }
    puts "Moves log: #{@moves_log.join('')}"
    puts "Waterproof: #{@waterproof}"
    puts "Water level: #{@water + 1}"
    puts "Number of steps underwater: #{@steps_underwater}"
    puts "Water rises in: #{@next_flood}"
    puts "Score: #{@score}"
    @data.reverse.each do |line|
      puts line.join('')
    end
  end

  def formatted_draw
    puts @score
    @data.reverse.each do |line|
      puts line.join('')
    end
  end

  def finished?
    !!@finished
  end

  private 

  def win!
    @score += 50 * @lambdas
    finish('win')
  end

  def abort!
    @score += 25 * @lambdas
    finish('abort')
  end

  def lose!
    finish('were crushed')
  end

  def drown!
    finish('drowned')
  end

  def finish(reason)
    @finished = true
    unless @interactive
      formatted_draw
    else
      draw
      puts "You #{reason}! #{@score} points."
      exit
    end
  end

  def move_to(y, x)
    current_x, current_y = current_position

    if [' ', '.'].include? @data[y][x]
      @data[y][x] = 'R'
      @data[current_y][current_x] = ' '

    elsif @data[y][x] == 'O'
      @data[y][x] = 'R'
      @data[current_y][current_x] = ' '
      win!

    elsif @data[y][x] == "\\"
      @score += 25
      @lambdas += 1
      @data[y][x] = 'R'
      @data[current_y][current_x] = ' '

    elsif @data[y][x] == '*'
      if y == current_y 
        if current_x > x && @data[y][x - 1] == ' '
          @data[y][x] = 'R'
          @data[current_y][current_x] = ' '
          @data[y][x - 1] = '*'
        elsif current_x < x && @data[y][x + 1] == ' '
          @data[y][x] = 'R'
          @data[current_y][current_x] = ' '
          @data[y][x + 1] = '*'
        end
      end

    end
  end

  def tick
    old_state = clone
    old_state.each_with_index do |line, y|
      line.each_with_index do |char, x|
        
        if char == '*' && old_state[y - 1][x] == ' '
          @data[y - 1][x] = '*'
          @data[y][x] = ' '
          check_crushing(y - 2, x)
        
        elsif char == '*' && old_state[y - 1][x] == "\\" && old_state[y-1][x+1] == ' ' && old_state[y][x+1] == ' '
          @data[y - 1][x + 1] = '*'
          @data[y][x] = ' '
          check_crushing(y - 2, x + 1)
        
        elsif char == '*' && old_state[y - 1][x] == "*" && old_state[y-1][x+1] == ' ' && old_state[y][x+1] == ' '
          @data[y - 1][x + 1] = '*'
          @data[y][x] = ' '
          check_crushing(y - 2, x + 1)

        elsif char == '*' && old_state[y - 1][x] == "*" && old_state[y-1][x-1] == ' ' && old_state[y][x-1] == ' '
          @data[y - 1][x - 1] = '*'
          @data[y][x] = ' '
          check_crushing(y - 2, x - 1)

        elsif char == 'L' && open_lift?
          @data[y][x] = 'O'
        end
      end
    end
    adjust_flood
  end
  
  def adjust_flood
    check_drowning
    unless @flooding == 0
      @next_flood -= 1
      if @next_flood == 0
        @water += 1
        @next_flood = @flooding
      end
    end
  end

  def check_crushing(y, x)
    current_x, current_y = current_position
    if current_x == x && current_y == y
      lose!
    end
  end

  def check_drowning
    if robot_underwater?
      drown! if @steps_underwater >= @waterproof
      @steps_underwater += 1
    else
      @steps_underwater = 0
    end
  end

  def robot_underwater?
    current_x, current_y = current_position
    current_y <= @water
  end

  def open_lift?
    @data.each do |line|
      return false if line.index("\\")
    end
    true
  end

  def parse_board(file)
    result = []
    file.each_line do |line|
      break if line == "\n"
      result << line.split('')
    end
    @data       = result.reverse
    begin
      @water      = file.readline.split[1].to_i - 1
      @flooding   = file.readline.split[1].to_i
      @waterproof = file.readline.split[1].to_i
    rescue EOFError
      @water      = -1
      @flooding   = 0
      @waterproof = 10
    end
  end

  def current_position
    @data.each_with_index do |line, y|
      if idx = line.index('R')
        return [idx, y]
      end
    end
  end

  def clone
    new_board = []
    @data.each_with_index do |line,idx|
      new_board[idx] = []
      line.each do |char|
        new_board[idx] << char.clone
      end
    end
    new_board
  end

end