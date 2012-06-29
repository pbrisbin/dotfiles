require 'rubygems'
begin
  require 'pp'
  require 'wirble'
  Wirble.init
  Wirble.colorize
rescue
  $stdout << "Error: Couldn't load the wirble or pp gems. Please install them to get the most benefit."
end

IRB.conf[:AUTO_INDENT] = false
ENV['CONSOLE'] = 'true'

class Object
  # get all the methods for an object that aren't basic methods from Object
  def my_methods
    (methods - Object.instance_methods).sort
  end
end

# from http://themomorohoax.com/2009/03/27/irb-tip-load-files-faster
def ls
  %x{ls}.split("\n")
end

def cd(dir)
  Dir.chdir(dir)
  Dir.pwd
end

def pwd
  Dir.pwd
end

alias :p :pp

module Readline
  module History
    LOG = "#{ENV['HOME']}/.irb_session_history"

    def self.write_log(line)
      File.open(LOG, 'ab') {|f| f << "#{line}
"}
    end

    def self.start_session_log
      write_log("
# session start: #{Time.now}

")
      at_exit { write_log("
# session stop: #{Time.now}
") }
    end
  end

  alias :old_readline :readline
  def readline(*args)
    ln = old_readline(*args)
    begin
      History.write_log(ln)
    rescue
    end
    ln
  end
end

Readline::History.start_session_log
