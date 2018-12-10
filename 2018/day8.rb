class Node
  attr_accessor :value
  attr_reader :children, :metadata

  def initialize(children, metadata)
    @children = children
    @metadata = metadata
    @value = nil
  end
end


def parse_node(data, i)
  num_chld = data[i]; i += 1
  num_meta = data[i]; i += 1

  children = []
  (1..num_chld).each do |ch|
    child, i = parse_node(data, i)
    children.push(child)
  end

  meta = []
  (1..num_meta).each do |m|
    meta.push(data[i]); i += 1
  end

  return [Node.new(children, meta), i]
end


def dfs_set_value(node, f)
  node.children.each do |ch|
    dfs_set_value(ch, f)
  end

  node.value = f.call(node)
end


def load_tree(filename)
  # let
  data =
    File.
    open(filename).
    read().
    split(' ').
    map { |s| s.to_i }

  tree, _ = parse_node(data, 0)

  # in
  tree
end


def solution_1(filename)
  tree = load_tree(filename)

  dfs_set_value(tree, lambda { |node|
    vchildren = node.children.map { |ch| ch.value }.reduce(0, :+)
    vnode = node.metadata.reduce(:+)

    vchildren + vnode
  })

  return tree.value.to_s
end


def solution_2(filename)
  tree = load_tree(filename)

  dfs_set_value(tree, lambda { |node|
    value = 0
    if node.children.length == 0
      value = node.metadata.reduce(:+)
    else
      node.metadata.each do |i|
        if i > 0 && node.children[i-1] != nil
          value += node.children[i-1].value
        end
      end
    end

    value
  })

  return tree.value.to_s
end


puts "Day 8/1: " + solution_1('day8.input')
puts "Day 8/2: " + solution_2('day8.input')
