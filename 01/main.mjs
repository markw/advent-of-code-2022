import * as fs from 'fs';

const take = (a,n) => a.slice(0,n);
const first = a => a[0];
const last = a => a[a.length-1];
const sum = (a,b) => a+b;
const reverseOrder = (a,b) => a < b ? 1 : (a > b) ? -1 : 0;
const readLines = fileName => fs.readFileSync(fileName, "utf-8").split("\n")

const processLine = ({score,scores}, line)=> {
  if (line) {
    score += Number(line);
  }
  else {
    scores.push(score);
    score = 0;
  }
  return ({score, scores});
};

const scores = readLines("input.txt") 
  .reduce(processLine, {score:0,scores:[]})
  .scores
  .sort(reverseOrder);

console.log("part 1:", first(scores));
console.log("part 2:", take(scores,3).reduce(sum));
