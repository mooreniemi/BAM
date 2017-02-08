require 'rantly/rspec_extensions'
require './kth_element'

describe KthElement do
  it 'gives Array a .find method' do
    expect(KthElement.respond_to?(:find)).to be true
  end
  it 'gives Array a .cheat method' do
    expect(KthElement.respond_to?(:cheat)).to be true
  end

  describe '.cheat' do
    it 'returns kth element statistic in O(n log n)' do
      expect(KthElement.cheat([2,1,3], 2)).to eq(2)
    end
  end

  describe '.find' do
    it 'returns kth element statistic in O(n)' do
      expect(KthElement.find([2,1,3], 2)).to eq(2)
    end
    it 'returns same answers as .cheat for same inputs' do
      property_of {
        len = Rantly { call([:range,1,100]) }
        array(len){integer}
      }.check { |arr|
        expect(KthElement.find(arr, 2)).to eq(KthElement.cheat(arr, 2))
      }
    end
  end
end
