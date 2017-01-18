// KthElement in this case means Kth order statistic
// for example, in an array like [33,4,22,0,9,5],
// the 1st order statistic (if we're starting at 1) is 0
// the 2nd order statistic would be 4, 3rd is 5, & so on
// but our goal is to do better than the cheat function
// while still maintaining its correctness
var KthElement;

KthElement = {
  find: function(array, element) {
    return element;
  },
  cheat: function(array, element) {
    return array.sort()[element - 1];
  }
};

module.exports = KthElement;
