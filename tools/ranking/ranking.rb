#!/usr/bin/env ruby
# usage: ranking.rb [options]
# options:
# -t, --timeout <seconds>
# -l, --lifter <path>
# -v, --validator <path>
# -m, --map <name>
# -f, --format -- brief, stable machine-readable output format

BASEDIR = File.dirname(__FILE__)
$: << BASEDIR
TOPDIR = File.join(BASEDIR, "..", "..")
RUNNER = File.join(BASEDIR, "runner.sh")

require 'getoptlong'
require 'icfp2012'
require 'open4'

opts = GetoptLong.new(
  [ '--timeout', '-t', GetoptLong::REQUIRED_ARGUMENT ],
  [ '--lifter', '-l', GetoptLong::REQUIRED_ARGUMENT ],
  [ '--validator', '-v', GetoptLong::REQUIRED_ARGUMENT ],
  [ '--map', '-m', GetoptLong::REQUIRED_ARGUMENT ],
  [ '--format', '-f', GetoptLong::NO_ARGUMENT ]
)

timelimit = 150
lifter = File.join(TOPDIR, 'bin', 'lifter')
validator = File.join(TOPDIR, 'bin', 'validator')

ranking = ICFP2012::WebRanking.get
maps = ranking.keys

$mformat = false

opts.each do |opt, arg|
  case opt
  when '--timeout'
    timelimit = arg.to_i
  when '--lifter'
    lifter = arg
  when '--validator'
    validator = arg
  when '--map'
    maps = [arg]
  when '--format'
    $mformat = true
  end
end

def locate_map map
  File.join(TOPDIR, 'tests', map) + '.map'
end

class Lifter
  WAITTIME=10

  def initialize binary, timelimit
    @binary = binary
    @timelimit = timelimit
  end
  
  def run map
    Open4.popen4 @binary, 'r+' do |pid, stdin, stdout, _|
      stdin.print map
      stdin.close
      result = IO.select([stdout], [], [], @timelimit)
      if result.nil?
        print ($mformat ? "!" : "[timeout]")
        Process.kill('INT', pid)
        IO.select([stdout], [], [], WAITTIME)
        Process.kill('KILL', pid)
      end
      return (stdout.gets || "")
    end
  end
end

lifter = Lifter.new lifter, timelimit

def rank_score ranking, score
  rank = 1
  ranking.each do |xs|
    sc, count = xs
    rank += count if sc > score
  end
  rank
end

maps.sort.each do |map|
  print(($mformat ? "%s " : "%12s ") % map)
  map_file = locate_map map
  soln = (lifter.run File.read(map_file)).strip
  score = `echo #{soln} | #{validator} -v #{map_file}`.to_i
  rank = rank_score ranking[map], score
  puts(($mformat ? "%d [%d]" : "%5d (rank %4d)") % [score, rank])
end
