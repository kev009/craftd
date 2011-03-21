require 'rake'
require 'thread'

module Rake
  # raised when getting a variable that has not be assigned
  class UndefinedVariable < Exception; end
  
  # raised when setting a variable for the second time
  class VariableAlreadyDefined < Exception; end
  
  class Variable
    class << self
      def vars
        @vars ||= Hash.new
      end
      
      def mutex
        # Use a mutex for getting & setting each var
        @mutex ||= Hash.new(Mutex.new)
      end
      
      # Based on Rake::TaskManager#lookup
      # Lookup a variable, using the intial_scope to hint at the full name.
      # If the var begins with 'rake:', only look in the top-level scope.
      # Prepend the var with '^' to search a higher scope.
      # Returns nil if the var cannot be found.
      def get(var, initial_scope=[])
        var = var.to_s
        if var =~ /^rake:/
          scopes = []
          var = var.sub(/^rake:/, '')
        elsif var =~ /^(\^+)/
          scopes = initial_scope[0, initial_scope.size - $1.size]
          var = var.sub(/^(\^+)/, '')
        else
          scopes = initial_scope
        end
        get_in_scope(var, scopes)
      end
      
      # Private method to find the variable in each scope
      def get_in_scope(var, scope)
        n = scope.size
        while n >= 0
          vn = (scope[0,n] + [var]).join(':')
          value = nil
          mutex[vn].synchronize { value = vars[vn] }
          return value if value
          n -= 1
        end
        raise UndefinedVariable, "Undefined variable '#{var}' in scope: #{scope.inspect}"
      end
      private :get_in_scope
      
      # Set the variable
      def set(var, value, scope=nil)
        k = ([scope] + [var]).flatten.compact.join(':')
        mutex[k].synchronize do
          raise(VariableAlreadyDefined, "#{k} is already defined") if vars.key?(k)
          vars[k] = value
        end
      end
      
    end
  end

  # Add +get+ and +set+ methods on Rake::NameSpace and Rake::Task
  module Extensions
    def get(var)
      Rake::Variable.get(var, @scope)
    end

    def set(var, value)
      Rake::Variable.set(var, value, @scope)
    end
    
    def method_missing(meth, *args)
      var = meth.id2name
      if var =~ /=$/ && 1 == args.size
        self.set(var.chop!, args.first)
      else
        self.get(var)
      end
    end
  end
  Rake::NameSpace.send(:include, Extensions)
  Rake::Task.send(:include, Extensions)

  # Define universal get & set methods on all objects
  Object.send(:define_method, :get) { |k| Rake::Variable.get(k) }
  Object.send(:define_method, :set) { |k,v| Rake::Variable.set(k,v) }
  # Object.send(:define_method, :method_missing) do |meth|
  #   begin
  #     get(meth.id2name)
  #   rescue Rake::UndefinedVariable
  #     super
  #   end
  # end
end
