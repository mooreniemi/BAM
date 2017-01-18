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
