#!/usr/bin/env ruby
# usage: ranking.rb [timeout] [lifter] [validator]

BASEDIR = File.dirname(__FILE__)
$: << BASEDIR
TOPDIR = File.join(BASEDIR, "..", "..")
RUNNER = File.join(BASEDIR, "runner.sh")

require 'icfp2012'
require 'open4'

TIMEOUT=(ARGV[0] || 150).to_i
lifter = ARGV[1] || File.join(TOPDIR, 'bin', 'lifter')
validator = ARGV[2] || File.join(TOPDIR, 'bin', 'validator')
ranking = ICFP2012::WebRanking.get
maps = ranking.keys

def locate_map map
  File.join(TOPDIR, 'tests', map) + '.map'
end

class Lifter
  WAITTIME=10

  def initialize binary
    @binary = binary
  end
  
  def run map
    Open4.popen4 @binary, 'r+' do |pid, stdin, stdout, _|
      stdin.print map
      stdin.close
      result = IO.select([stdout], [], [], TIMEOUT)
      if result.nil?
        STDERR.print "[timeout]"
        Process.kill('INT', pid)
        IO.select([stdout], [], [], WAITTIME)
        Process.kill('KILL', pid)
      end
      return (stdout.gets || "")
    end
  end
end

lifter = Lifter.new lifter

def rank_score ranking, score
  rank = 1
  ranking.each do |xs|
    sc, count = xs
    rank += count if sc > score
  end
  rank
end

maps.sort.each do |map|
  print("%12s " % map)
  map_file = locate_map map
  soln = (lifter.run File.read(map_file)).strip
  score = `echo #{soln} | #{validator} -v #{map_file}`.to_i
  rank = rank_score ranking[map], score
  puts("%5d (rank %4d)" % [score, rank])
end
