'use strict';

var fs = require('fs');
var gnuplot = require('./node_modules/gnuplot/gnuplot.js');
var json2csv = require('./node_modules/json2csv/dist/json2csv.js');
var KthElement = require('./kth-element.js');

function timePerformanceOf(doSomething, times, inputGenerator) {
  var runs = [];
  for (var i = 0; i < times; i++) {
    var input = inputGenerator.call(null, i);
    var t = process.hrtime();
    doSomething(input);
    t = process.hrtime(t);
    var seconds = t[0] + (t[1] / 1000000000);
    runs.push(seconds);
    times--;
  }

  var avg = runs.reduce(function(a,b) { return a + b; }, 0) / runs.length;
  console.log('took on average %d seconds', avg);
  return runs;
};

function generateArray(size) {
  return new Array(10 * size);
};

var TIMES = 5000;
var x = timePerformanceOf(KthElement.find, TIMES, generateArray);
var x2 = timePerformanceOf(KthElement.cheat, TIMES, generateArray);
var y = Array.apply(null, {length: x.length}).map(function(_,x) { return 10 * x; });

var chart = x.map(function (e, i) {
  return {
    find: e,
    cheat: x2[i],
    inputSize: y[i]
  };
});

var fields = ['find', 'cheat', 'inputSize'];

var text = json2csv({ data: chart, fields: fields, hasCSVColumnTitle: false });
fs.writeFile("./data/kth-element.csv", text, function(err) {
  if(err) {
    return console.log(err);
  }

  console.log("The file was saved! Now plotting...");

  gnuplot().
    set('term png').
    set('output "data/kth-element.png"').
    set('title "Performance of Kth Element"').
    set('xlabel "input size"').
    set('ylabel "execution time (seconds)"').
    set('datafile separator ","').
    plot('"data/kth-element.csv" using 3:1 w l title "find", "data/kth-element.csv" using 3:2 w l title "cheat"').
    end();
});
