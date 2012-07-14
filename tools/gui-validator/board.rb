class Board
  attr_reader :data
  attr_reader :moves_log
  attr_reader :score

  def initialize(params)
    @data = parse_board(params)
    @moves_log = []
    @score = 0
    @lambdas = 0
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
      abort
    end
    tick
  end

  def draw
    50.times { puts '' }
    puts "Score: #{@score}"
    @data.reverse.each do |line|
      puts line.join('')
    end
  end

  private 

  def move_to(y, x)
    current_x, current_y = current_position

    if [' ', '.'].include? @data[y][x]
      @data[y][x] = 'R'
      @data[current_y][current_x] = ' '

    elsif @data[y][x] == 'O'
      @score += 50 * @lambdas
      puts "You won! #{@score} points."
      exit

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
  end

  def check_crushing(y, x)
    current_x, current_y = current_position
    if current_x == x && current_y == y
      puts "Robot crushed. #{@score} points."
      exit
    end
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
      result << line.split('')
    end
    result.reverse
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

  def abort
    @score += 25 * @lambdas
    puts "Mission aborted. #{@score} points."
    exit
  end

end