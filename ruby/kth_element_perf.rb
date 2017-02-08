require 'graph/function'
require './kth_element'

Graph::Function.as_gif(File.expand_path('../../data/kth-element.gif', __FILE__))

def find(a)
  KthElement.find(a, 2)
end

def cheat(a)
  KthElement.find(a, 2)
end

Graph::Function::IntsComparison.of(method(:find), method(:cheat))
