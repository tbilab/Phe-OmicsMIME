// number formatters
const countFormat = d3.format(",d");
const CiFormat = d3.format(".3f");
const pValFormat = d3.format("0.2");

// Hardcoded settings
const highlightColor = '#fdcdac'; // Color of the highlight bars
const margin = {right: 20, left: 10, top: 20, bottom: 100}; // margins on side of chart

// layout grid
const proportionPlotUnits = 0; // width of proportion CIs
const matrixPlotUnits = 2;
const countBarUnits = 3;
const marginalChartRatio = 0.2;  // what proportion of vertical space is the code marginal count bars?

// matrix settings
const matrixPadding = 5;              // How far in from sides the dots start
const matrixSize = 7;                  // radius of dots
const matrixPresentColor = 'black'; 
const matrixMissingColor = 'lightgrey';

// proportion plot settings
const propPointSize = 0; // size of the point estimates for proportions
const ciThickness = 4;   // how thick is the CI?

// maginal bars settings
const marginalBottomPadding = 5;  // padding between matrix and start of bars
const marginalBarPadding = 0.5;

// count bar settings
const countBarPadding = 0.5;  // vertical gap between count bars.
const countBarLeftPad = 35; // how much space on left of count bars do we leave for popup info?


// Calculated constants
const h = height - margin.top - margin.bottom;
const w = width - margin.left - margin.right;
const totalWidthUnits = proportionPlotUnits + matrixPlotUnits + countBarUnits;
const proportionPlotWidth = w*(proportionPlotUnits/totalWidthUnits);
const matrixPlotWidth = w*(matrixPlotUnits/totalWidthUnits);
const countBarWidth = w*(countBarUnits/totalWidthUnits);
const marginalChartHeight = h*marginalChartRatio;
const matrixDotSize = Math.min(
  (matrixPlotWidth )/(data.length), 
  matrixSize
);

// empty old svg content
svg.html('');

// Parent padded g element.
const padded = svg.append('g')
  .translate([margin.left, margin.top]);

// get unique codes present in the data.
const codeList = Object.keys(
  data
  .reduce(
    (all, current) => [...all, ...(current.pattern.split('/'))],
    []
  ).reduce(
    (codeDict, currentCode) => Object.assign(codeDict, {[currentCode]: 1}),
    {}
  )
);


// ----------------------------------------------------------------------
// Scales
// ----------------------------------------------------------------------
const matrixWidthScale = d3.scaleBand()
  .domain(codeList)
  .range([matrixPadding,matrixPlotWidth - matrixPadding])
  .round(true)
  .padding(0.05); // goes from right to left into left margin.
  
const horizontalSpace = matrixWidthScale.bandwidth();

const marginBarWidth = horizontalSpace - 2*marginalBarPadding;

const countX = d3.scaleLinear()
  .range([countBarWidth, countBarLeftPad])
  .domain([0, d3.max(data, d=> d.count)]);

const proportionX = d3.scaleLinear()
  .range([0,proportionPlotWidth])
  .domain([0,d3.max(data, d => d.upper)]);
  
const y = d3.scaleLinear()
  .range([marginalChartHeight, h])
  .domain([0, data.length]);

const verticalSpace = y(1) - y(0);   // how big of a gap each pattern gets
const barHeight = verticalSpace - 2*countBarPadding;

const marginalY = d3.scaleLinear()
  .range([marginalChartHeight-marginalBottomPadding, 0])
  .domain([0, d3.max(options.marginalData, d => d.count)]);

// ----------------------------------------------------------------------
// Chart Components
// ----------------------------------------------------------------------
const matrixChart = padded.append('g.matrixChart')
  .translate([countBarWidth,0]);
// Variable to keep track of the currently highlighted count bar
let currentHighlighted = null;

matrixChart.selectAll('.currentRow')
  .data(data)
  .enter().append('g.currentRow')
  .translate((d,i) => [0, y(i)] )
  .each(function(currentEntry, i){
    
    // Initially hidden box for highlighting purposes. 
    const highlightRect = d3.select(this)
      /*.selectAppend('rect.highlightRect')*/
      .append('rect')
      .attr('class', 'highlightRect hoverInfo') // Use both classes for styling and selection
      .at({
        width: w + 20,
        x: -(countBarWidth + countBarPadding*2),
        height: barHeight + countBarPadding,
        fillOpacity: 0.3,
        fill: highlightColor,
        stroke: 'black',
        rx: 5,
        opacity: 0,
        'class': 'hoverInfo'
      });
    
    // Matrix key
    const matrixRow = d3.select(this).append('g.matrixRow');

    const allCodes = matrixRow
      .selectAll('.allCodes')
      .data(codeList)
      .enter().append('circle')
      .attr('class', 'allCodes')
      .at({
        cx: d => matrixWidthScale(d) + matrixWidthScale.bandwidth()/2, 
        cy: verticalSpace/2,
        r: matrixDotSize, 
        fill: matrixMissingColor,
        fillOpacity: 0.3,
      });
    
    // bars that go accross
    const codePositions = currentEntry.pattern
      .split('/')
      .map(d => matrixWidthScale(d) + matrixWidthScale.bandwidth()/2);
    
    const rangeOfPattern = d3.extent(codePositions)

    matrixRow.append('line')
      .at({
        x1: rangeOfPattern[0],
        x2: rangeOfPattern[1],
        y1: verticalSpace/2,
        y2: verticalSpace/2,
        stroke: matrixPresentColor,
        strokeWidth: matrixDotSize/2
      })
    
    const presentCodes = matrixRow
      .selectAll('.presentCodes')
      .data(codePositions)
      .enter().append('circle')
      .attr('class', 'presentCodes')
      .at({
        cx: d => d, 
        cy: verticalSpace/2,
        r: matrixDotSize, 
        fill: matrixPresentColor,
      });
      
      
    // Count Bars
    const countBar = d3.select(this).append('g.countBar')
      .translate([-countBarWidth,0]);
    
    countBar.append('rect')
      .at({
        fill: 'steelblue',
        height: barHeight,
        x: countX(currentEntry.count),
        y: countBarPadding/2,
        width: countX(0) - countX(currentEntry.count),
      })


    
    countBar.append('text')
      .text(countFormat(currentEntry.count))
      .at({
        x: countX(currentEntry.count) - 1,
        y: -y(i) + marginalChartHeight - 28,
        alignmentBaseline: 'middle',
        textAnchor: 'end',
        fontWeight: 'bold',
        opacity: 0,
        'class': 'hoverInfo'
      })
      
     countBar.append('line')
      .at({
        x1: countX(currentEntry.count),
        x2: countX(currentEntry.count),
        y1: -y(i) + marginalChartHeight - 28,
        y2: -y(i) + marginalChartHeight,
        stroke: 'black',
        opacity: 0,
        'class': 'hoverInfo'
      })

  })
  .on('mouseover', function(d){
    d3.select(this).selectAll('.hoverInfo').attr('opacity', 1)
  })
  .on('mouseout', function(d){
     if (this !== currentHighlighted) {
    d3.select(this).selectAll('.hoverInfo').attr('opacity', 0)
     }
  })
  .on('click', function(d, i) {
    resetHighlights(); // Reset highlights for both count and marginal bars
    
    // If there's a currently highlighted count bar, reset its highlightRect opacity
    if (currentHighlighted) {
      d3.select(currentHighlighted).selectAll('.hoverInfo').attr('opacity', 0);
    }

    // Highlight the clicked count bar by showing its highlightRect
    d3.select(this).select('.highlightRect').attr('opacity', 1);

    // Update the reference to the currently highlighted count bar
    currentHighlighted = this;
    //send to shiny
    sendClickedPatternToShiny(d.pattern);
  })
// ----------------------------------------------------------------------
// Axes
// ----------------------------------------------------------------------

const matrixAxis = matrixChart.append("g")
  .call(d3.axisBottom().scale(matrixWidthScale))
  .translate([0, h]);

matrixAxis
  .selectAll("text")
  .at({
    x: -7,
    y: -1,
    textAnchor: 'end',
    transform: 'rotate(-35)',
    fontSize:12
  });
    
matrixAxis.select('.domain').remove()

const countAxis = padded.append('g.countAxis')
 .translate([0, marginalChartHeight - marginalBottomPadding]);

countAxis.append("g")
  .call(d3.axisTop().scale(countX).ticks(5).tickSizeOuter(0))
  .selectAll("text")
  .at({
    x: -2,
    textAnchor: 'end',
    opacity: 0.5
  });
  
countAxis.select('.tick').select('line')
  .at({
    y1: h-marginalChartHeight
  });
  
countAxis.append('text')
  .at({
    x: countBarWidth/2,
    y: h - marginalChartHeight+ 20,
  })
  .classed('axisTitles', true)
  .text('Set size')


const marginalCountAxis = padded.append("g")
  .translate([countBarWidth,0])
  .call(d3.axisLeft().scale(marginalY).ticks(4).tickSizeOuter(0));

marginalCountAxis.selectAll("text")
  .attr('text-anchor', 'end')
  .attr('opacity', 0.5);

marginalCountAxis.select('text').remove() // hides the first zero so we can double use the one from the proportion chart. Hacky. 

// ----------------------------------------------------------------------
// Marginal bars. 
// ----------------------------------------------------------------------
const marginalCountsChart = padded.append('g.marginalCountsChart')
  .translate([countBarWidth,0]);

let currentMarginalHighlighted = null; // Track the currently highlighted marginal bar

const marginalBars = marginalCountsChart.selectAll('.marginalCounts')
  .data(options.marginalData)
  .enter().append('g')
  .attr('class', 'marginalCounts')
  .translate(d => [matrixWidthScale(d.code), marginalY(d.count)])
  .on('mouseover',function(d){
    d3.select(this).selectAll('.margingMouseoverInfo').attr('opacity', 1);
  })
  .on('mouseout', function(d) {
    if (this !== currentMarginalHighlighted) { // Only reset opacity if it's not the currently selected marginal bar
      d3.select(this).selectAll('.margingMouseoverInfo').attr('opacity', 0);
    }
  })
  .on('click', function(event, d) {
    resetHighlights(); // Reset highlights for both count and marginal bars
    
    // Reset highlight of previously selected marginal bar, if any
    if (currentMarginalHighlighted && currentMarginalHighlighted !== this) {
      d3.select(currentMarginalHighlighted).selectAll('.margingMouseoverInfo').attr('opacity', 0);
    }

    // Highlight the clicked marginal bar by showing its .margingMouseoverInfo elements
    d3.select(this).selectAll('.margingMouseoverInfo').attr('opacity', 1);

    // Update reference to currently highlighted marginal bar
    currentMarginalHighlighted = this;
  })

marginalBars.append('rect')
  .at({
    height: d => marginalY(0) - marginalY(d.count),
    width: matrixWidthScale.bandwidth(),
    fill: 'orangered'
  })
  
marginalBars.append('rect')
  .at({
    y: d => -marginalY(d.count)-marginalBottomPadding,
    height: h,
    width: matrixWidthScale.bandwidth(),
    fillOpacity: 0.3,
    fill: highlightColor,
    stroke: 'black',
    rx: 5,
    opacity: 0,
    "class": "margingMouseoverInfo"
  })
  
  
marginalBars.append('text')
  .text(d => countFormat(d.count))
  .at({
    y: 0,
    x: d => -matrixWidthScale(d.code) - 20,
    textAnchor: 'end',
    fontWeight: 'bold',
    opacity: 0,
    "class": "margingMouseoverInfo"
  })
  
marginalBars.append('line')
  .text(d => countFormat(d.count))
  .at({
    y1: 0,y2:0,
    x1: d => -matrixWidthScale(d.code) - 20,
    x2: d => -matrixWidthScale(d.code),
    stroke: 'black',
    opacity: 0,
    "class": "margingMouseoverInfo"
  })
  
marginalBars.append('text')
  .html(d => `<tspan>Code:</tspan> ${d.code}`)
  .at({
    y: d => -marginalY(d.count) + marginalChartHeight/3,
    x: d => -matrixWidthScale(d.code) - countBarWidth,
    fontSize: 24,
    textAnchor: 'start',
    opacity: 0,
    "class": "margingMouseoverInfo"
  })
  
function resetHighlights() {
    // Reset highlight for marginal bars
    if (currentMarginalHighlighted) {
        d3.select(currentMarginalHighlighted).selectAll('.margingMouseoverInfo').attr('opacity', 0);
        currentMarginalHighlighted = null; // Clear the reference
    }

    // Reset highlight for count bars (assuming similar logic and variable naming)
    if (currentHighlighted) {
        d3.select(currentHighlighted).selectAll('.hoverInfo').attr('opacity', 0);
        currentHighlighted = null; // Clear the reference
    }
}


