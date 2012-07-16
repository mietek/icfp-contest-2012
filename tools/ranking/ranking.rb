#!/usr/bin/env ruby
require "rubygems"
require "bundler/setup"

require 'gli'

require 'icfp2012'

include GLI

command :update do |c|
  ranking = ICFP2012::WebRanking.get
  p ranking
end
