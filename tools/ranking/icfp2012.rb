#!/usr/bin/env ruby

module ICFP2012
  module WebRanking
    URL = 'http://www.undecidable.org.uk/~edwin/cgi-bin/weblifter.cgi?standings=1'
#    URL = '/tmp/standings'
    
    def self.get
      require 'nokogiri'
      require 'open-uri'
      
      ranking = {}
      
      doc = Nokogiri::HTML(open(URL))
      
      doc.css("h4").each do |h4|
        map = h4.text
        ranking[map] = []
        scores = h4.next.text.split(', ')
        scores.map do |score|
          score =~ /(\d+)( \((\d+) times\))?/
          ranking[map].push [$1.to_i, ($3 || 1).to_i]
        end
      end
      
      ranking
    end
  end
end
