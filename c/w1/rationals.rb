class MyRational
  def initialize(numer, denom = 1)
    if denom == 0
      raise "Denominator can't be zero"
    elsif denom < 0
      @numer = -numer
      @denom = -denom
    else
      @numer = numer
      @denom = denom
    end
    reduce
  end

  def to_s
    "#{@numer}#{if @denom == 1 then "" else "/" + @denom.to_s end}"
  end

  def add! r
    a = r.numer
    b = r.denom
    c = @numer
    d = @denom
    
    @numer = (a * d) + (b * c)
    @denom = b * d
    
    reduce
    self
  end

  def + r
    ans = MyRational.new(@numer, @denom)
    ans.add! r
  end

  protected

  def numer
    @numer
  end

  def denom
    @denom
  end

  private

  def gcd(x, y)
    if x == y
      x
    elsif x < y
      gcd(x, y - x)
    else
      gcd(y, x)
    end
  end

  def reduce
    if @numer == 0
      @denom = 1
    else
      d = gcd(@numer.abs, @denom)
      @numer = @numer / d
      @denom = @denom / d
    end
  end
end

def use_rationals
  r1 = MyRational.new(3, 4)
  r2 = r1 + r1 + MyRational.new(-5, 2)

  puts r2.to_s
end

use_rationals
