#!/usr/bin/env rake

require 'listen'

def number_of_cores
  `sysctl -n hw.ncpu`.chomp # Only for Mac OS X
end

def build_flags
  "-j#{number_of_cores}"
end

def exit_with_int
  Signal.trap 'INT' do
    exit
  end
end

desc "Run tests"
task :test do
  sh "cabal install #{build_flags} --only-dependencies --enable-tests"
  sh "cabal configure --enable-tests"
  sh "cabal build"
  sh "cabal test"
end

task :doc => %w|doc:build doc:open|
namespace :doc do
  desc "Generate haddock documentation"
  task :build do
    sh "cabal install #{build_flags} --only-dependencies"
    sh "cabal configure"
    sh "cabal build"
    sh "cabal haddock"
  end
  desc "Open document"
  task :open do
    sh "open dist/doc/html/hi/index.html"
  end
end

namespace :release do
  desc "Check releasable or not"
  task :check do
    sh './make_sdist.sh'
  end
end

desc "Run tests"
task :default => :test
