#!/usr/bin/env ruby
BASEDIR = File.dirname(__FILE__)
TOPDIR = File.join(BASEDIR, "..", "..")
RUNNER = File.join(BASEDIR, "runner.sh")

require 'icfp2012'
require 'open3'

lifter = ARGV[0] || File.join(TOPDIR, 'bin', 'lifter')
validator = ARGV[1] || File.join(TOPDIR, 'bin', 'validator')
ranking = ICFP2012::WebRanking.get
maps = ranking.keys

def locate_map map
  File.join(TOPDIR, 'tests', map) + '.map'
end

class Lifter
  TIMEOUT=10
  WAITTIME=10

  def initialize binary
    @binary = binary
  end
  
  def run map
    Open3.popen3 @binary, 'r+' do |stdin, stdout, _, thr|
      stdin.print map
      stdin.close
      result = IO.select([stdout], [], [], TIMEOUT)
      if result.nil?
        puts "Timeout, sending INT..."
        Process.kill('INT', thr.pid)
        IO.select([stdout], [], [], WAITTIME)
        Process.kill('KILL', thr.pid)
      end
      stdout.gets
    end
  end
end

lifter = Lifter.new lifter

maps=['contest1']

maps.each do |map|
  puts "Evaluating #{map}..."
  map = File.read(locate_map map)
  soln = lifter.run map
  puts soln
end
