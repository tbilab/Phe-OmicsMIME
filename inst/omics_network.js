svg.selectAll("*").remove();
//svg.style('background-color', "#F2F3F6");
const padding = 20;
const X = d3.scaleLinear()
  .range([10, width - 10]);
  
const Y = d3.scaleLinear()
  .range([10, height-20]);
  
const nodes = HTMLWidgets.dataframeToD3(data.nodes);
const links = HTMLWidgets.dataframeToD3(data.edges);
//console.log(nodes);

const node_types = unique(nodes.map(d => d.type));
const num_types = node_types.length;

type_to_prop = {
  gene:  {r:3, opacity: 1, color: "#689030"},
  protein: {r:3, opacity: 1, color: "#5E738F"},
  metabolite: {r:3, opacity: 1, color: "#AD6F3B"},
  phecode: {r:4.7, opacity: 1, color: "#673770"}
};

///removing force-directed network
///begin to force-directed simulation
const simulation = d3.forceSimulation(nodes)
  .force(
    "link", 
    d3.forceLink(links)
      .id(d => d.id)
      .distance(100)
      .strength(0.8) // Increase the strength to make it stable
      //.iterations(2)
  )
  .force(
    "collision", 
    d3.forceCollide()
      .radius(d => type_to_prop[d.type].r+2)
      .iterations(2)
   )
  .force(
    "charge", 
    d3.forceManyBody()
     .strength(-8)
  ) 
  .on("tick", ticked);
// Run the simulation for a few iterations to stabilize the positions
for (let i = 0; i < 500; i++) {
  simulation.tick();
}

///create the network
const link = svg.append("g")
   .attr("stroke", "#999")
   .attr("stroke-opacity", 0.1)
   .selectAll("line")
   .data(links)
   .enter().append("line")
   .attr("stroke-width", 1.5)
   .attr("class", "edge"); // Add the 'edge' class to the edges;

const node = svg.append("g")
    .attr("stroke", "#fff")
  .selectAll("circle")
  .data(nodes)
  .enter().append("circle")
    .attr("r", d => type_to_prop[d.type].r)
    .attr("fill", d => type_to_prop[d.type].color)
    .attr('fill-opacity', d => type_to_prop[d.type].opacity)
    .call(drag(simulation));
// Define the size of pre-selected and non-selected nodes
const selectedNodeSize = 15; // Adjust the size for pre-selected nodes
// Add a CSS class for pre-selected nodes
node.filter(d => d.selected === 'yes')
  .classed('pre-selected-node', true)
  .attr('r', selectedNodeSize)

/// Add CSS styles for the "hovered" class
const hoverStyles = {
  cursor: 'pointer', // Change cursor to a hand
  r: selectedNodeSize, // Increase node size on hover
};
const unhoverStyles = {
  cursor: 'pointer', // Reset cursor
  r: d => type_to_prop[d.type].r, // Reset node size
};

// Make sure each node has a unique ID for selection
node.attr('id', d => `node-${d.id}`);

// Add event listeners to show and hide the table on hover
node.on('mouseover', function(d){
  //const isPreSelected = d.selected === 'yes';
  //const isSelected = selectedNodes.has(d);

  d3.select(this)
    //.classed('hovered', true) // Add the "hovered" class
    .style('cursor', hoverStyles.cursor) // Change cursor to a hand
    .attr('r', hoverStyles.r); // Apply hover styles
    
  // Show the table and populate it with node information
  tooltipDiv.transition()
    //.duration(200)
    .style("opacity", 1);
  tooltipDiv.html(generateTable(d))
    .style("left", (d3.event.pageX + 20) + "px")
    .style("top", (d3.event.pageY + 20) + "px");
    
  // Highlight connected edges
  link.filter(l => l.source === d || l.target === d)
        .style('stroke', 'black') // Change to black or any other color
        .style('stroke-width', '5px'); // Increase stroke-width to highlight
    
})
.on('mouseout', function(d){
  const isPreSelected = d.selected === 'yes';
  
  if (!isPreSelected) {
  d3.select(this)
    //.classed('hovered', false) // Remove the "hovered" class
    .style('cursor', unhoverStyles.cursor) // Reset cursor
    .attr('r', unhoverStyles.r); // Reset node size
  }
  if (isPreSelected | selectedNodes.has(d)) {
  d3.select(this)
    //.classed('hovered', false) // Remove the "hovered" class
    .style('cursor', "pointer") // Reset cursor
    .attr('r', selectedNodeSize); // Reset node size
  }

  // Hide the table when the mouse moves away
  tooltipDiv.transition()
    //.duration(500)
    .style("opacity", 0);
  
  // Reset connected edges to original style
  link.filter(l => l.source === d || l.target === d)
        .style('stroke', '#999') // Reset to original color
        .style('stroke-width', '1.5px'); // Reset to original stroke-width
    
});

// Legend data remains as previously defined
const legendData = [
  { label: "Gene", color: "#689030" },
  { label: "Protein", color: "#5E738F" },
  { label: "Metabolite", color: "#AD6F3B" },
  { label: "Phenotype", color: "#673770" }
];

// Starting position for the legend at the top-left with padding
const legendPadding = 10; // Padding around the legend
const legendStartX = legendPadding;
const legendStartY = height-15;
const legendItemSpacing = 100; // Spacing between legend items

// Create the legend group, positioned at the top-left with padding
const legend = svg.append("g")
  .attr("class", "legend")
  .attr("transform", `translate(${legendStartX}, ${legendStartY})`);

// Create legend items, arranging them horizontally with specified spacing
legend.selectAll("g")
  .data(legendData)
  .enter()
  .append("g")
    .attr("transform", (d, i) => `translate(${i * legendItemSpacing}, 0)`) // Layout items horizontally
    .each(function(d) {
      const g = d3.select(this);
      // Append rectangle for color
      g.append("rect")
        .attr("x", 0)
        .attr("width", 18)
        .attr("height", 18)
        .style("fill", d.color);

      // Append text label, positioned to the right of the rectangle
      g.append("text")
        .attr("x", 22) // Position text to the right of the rectangle
        .attr("y", 9) // Vertically center text with rectangle
        .attr("dy", "0.35em")
        .text(d.label);
    });

function ticked(){
  
  X.domain(d3.extent(nodes, d => d.x));
  Y.domain(d3.extent(nodes, d => d.y));
  
  link
      .attr("x1", d => X(d.source.x))
      .attr("y1", d => Y(d.source.y))
      .attr("x2", d => X(d.target.x))
      .attr("y2", d => Y(d.target.y));

  node
      .attr("cx", d => X(d.x))
      .attr("cy", d => Y(d.y));
}

function drag(simulation) {
  
  function dragstarted(d) {
    if (!d3.event.active) simulation.alphaTarget(0.3).restart();
    d.fx = d.x;
    d.fy = d.y;
  }
  
  function dragged(d) {
    d.fx = d3.event.x;
    d.fy = d3.event.y;
  }
  
  function dragended(d) {
    if (!d3.event.active) simulation.alphaTarget(0);
    d.fx = d3.event.x;
    d.fy = d3.event.y;
  }
  
  return d3.drag()
    .on("start", dragstarted)
    .on("drag", dragged)
    .on("end", dragended);
}

// Helpers
function unique(vec){
  return [...new Set(vec)];
}

////Begin to add more features based on the foced-directed graph

///adding node information table
// Create a div element to hold the table
const tooltipDiv = d3.select("body").append("div")
  .attr("class", "tooltip")
  .style("opacity", 0);
// Function to generate the HTML table for a node with styles
function generateTable(node) {
  return `
    <table style="font-size: 14px; border-collapse: collapse; width: 150px;">
      <tr style="background-color: grey; border: 1px solid #ddd;">
        <th style="padding: 8px; text-align: left;">ID</th>
      </tr>
      <tr style="background-color: white;border: 1px solid #ddd;">
        <td style="padding: 8px; text-align: left;">${node.id}</td>
      </tr>
      <tr style="background-color: grey; border: 1px solid #ddd;">
        <th style="padding: 8px; text-align: left;">Type</th>
      </tr>
      <tr style="background-color: white;border: 1px solid #ddd;">
        <td style="padding: 8px; text-align: left;">${node.type}</td>
      </tr>
    </table>
  `;
}
// Function to generate a simplified HTML tooltip with only the ID
function generateSimplifiedTooltip(node) {
  return `
    <div style="font-size: 12px; padding: 4px; background-color: white; border: 1px solid #ddd;">
      <strong>ID:</strong> ${node.id}
    </div>
  `;
}

// Define a set to store preselected nodes
const preselectedNodes = new Set(nodes.filter(d => d.selected === 'yes'));
sendPreSelectNodeToShiny(preselectedNodes);
//sendClickedNodeToShiny(preselectedNodes);

///add isolate and return
// Create a group for buttons
//const buttonGroup = svg.append("g")
  //.attr("transform", `translate(${width - 150}, 10)`);
// Determine the width of the SVG container
//const svgWidth = width; // You may need to adjust this based on your actual container width
//const svgHeight = height;
// Calculate the x-positions for the buttons to center them
//const isolateButtonX = svgWidth - 130;
//const isolateButtonY = svgHeight - 40;

// Create the Isolate button
//buttonGroup.append("rect")
//  .attr("x", isolateButtonX)
//  .attr("y", isolateButtonY)
//  .attr("width", 120)
//  .attr("height", 30)
//  .attr("rx", 5)
//  .attr("ry", 5)
//  .style("fill", "#D3D3D3")
//  .style("cursor", "pointer")
//  .on("click", isolateSelectedNode);
//buttonGroup.append("text")
//  .attr("x", isolateButtonX + 60)
//  .attr("y", isolateButtonY + 15)
//  .attr("text-anchor", "middle")
//  .attr("dy", "0.35em")
//  .style("fill", "black")
//  .style("font-size", "1.5rem")
//  .style("pointer-events", "none")
//  .text("Sub-network");

///change the node size of user pre-selected nodes
//const preSelectedNodes = nodes.filter(d => d.selected === 'yes');
// Create a set to keep track of selected nodes
const selectedNodes = new Set(); //does not include pre-selected nodes
// Filter nodes that are pre-selected
//const userPreSelectedNodes = new Set(preSelectedNodes); //include pre-selected nodes
// Set the size of pre-selected nodes
node
  .filter(d => d.selected === 'yes')
  .attr('r', selectedNodeSize)
  //.attr('fill', 'red')
  .attr("fill",d => type_to_prop[d.type].color);
// Set the size of non-selected nodes to their original size
node
  .filter(d => d.selected !== 'yes')
  .attr('r', d => type_to_prop[d.type].r)
  .attr("fill",d => type_to_prop[d.type].color);
  
///begin to define the actions when click the node
node.on('click', function (clickedNode) {
  
  // Check if the clicked node is user pre-selected
  // Toggle the selection state of the clicked node
  if (selectedNodes.has(clickedNode)) {
    selectedNodes.delete(clickedNode); //if click again, return to the previous state
  } else {
    selectedNodes.add(clickedNode);
  }

  // Change the color of selected nodes to red
  node.filter(d => selectedNodes.has(d))
    .attr('fill', 'red')
    .attr('r', 15);
  // Change the color of unselected nodes to their original color
  node.filter(d => !selectedNodes.has(d))
    .attr('fill', d => type_to_prop[d.type].color);

  // Highlight shared edges and change the color of unconnected nodes to grey
  link.attr('stroke', '#999'); // Reset all edges to their original color
  if (selectedNodes.size > 0) {
    link.filter(d => selectedNodes.has(d.source) && selectedNodes.has(d.target))
      .attr('stroke', "#DA5724");
    // Find nodes that are connected to all selected nodes
    const nodesConnectedToAllSelected = nodes.filter(node => isNodeConnectedToAllSelectedNodes(node));
    // Change the color of the edges connected to all selected nodes to blue
    link.filter(d => {
      const sourceConnectedToAll = isNodeConnectedToAllSelectedNodes(d.source);
      const targetConnectedToAll = isNodeConnectedToAllSelectedNodes(d.target);
      return (selectedNodes.has(d.source) && targetConnectedToAll) || (sourceConnectedToAll && selectedNodes.has(d.target));
    }).attr('stroke', "#DA5724"); // Change to your desired color

    // Change unconnected nodes to the desired grey color
    node.filter(d => !selectedNodes.has(d) && !isNodeConnectedToAllSelectedNodes(d))
      .attr('fill', '#888');
  }
  
  // Send the clicked node ID to your Shiny module using the module's namespace (ns)
  // Combine preselected and clicked nodes
  //const allSelectedNodes = new Set([...preselectedNodes, ...selectedNodes]);
  
  sendClickedNodeToShiny(selectedNodes);

  
});

Shiny.addCustomMessageHandler('updateD3Selection', function(message) {
  
    // Ensure message is treated as an array
    const updatedIds = Array.isArray(message) ? message : [message];
    
    // Clear the current selection
    selectedNodes.clear();
    // Update the selection with the new IDs
    updatedIds.forEach(id => {
        const node = nodes.find(n => n.id === id);
        if(node) {
            selectedNodes.add(node);
        }
    });
    
    // Change the color of selected nodes to red
  node.filter(d => selectedNodes.has(d))
    .attr('fill', 'red')
    .attr('r', 15);
  // Change the color of unselected nodes to their original color
  node.filter(d => !selectedNodes.has(d))
    .attr('fill', d => type_to_prop[d.type].color);

  // Highlight shared edges and change the color of unconnected nodes to grey
  link.attr('stroke', '#999'); // Reset all edges to their original color
  if (selectedNodes.size > 0) {
    link.filter(d => selectedNodes.has(d.source) && selectedNodes.has(d.target))
      .attr('stroke', "#DA5724");
    // Find nodes that are connected to all selected nodes
    const nodesConnectedToAllSelected = nodes.filter(node => isNodeConnectedToAllSelectedNodes(node));
    // Change the color of the edges connected to all selected nodes to blue
    link.filter(d => {
      const sourceConnectedToAll = isNodeConnectedToAllSelectedNodes(d.source);
      const targetConnectedToAll = isNodeConnectedToAllSelectedNodes(d.target);
      return (selectedNodes.has(d.source) && targetConnectedToAll) || (sourceConnectedToAll && selectedNodes.has(d.target));
    }).attr('stroke', "#DA5724"); // Change to your desired color

    // Change unconnected nodes to the desired grey color
    node.filter(d => !selectedNodes.has(d) && !isNodeConnectedToAllSelectedNodes(d))
      .attr('fill', '#888');
  }
    
    sendClickedNodeToShiny(selectedNodes);
    
});

///=======================functions===================
///===================================================
// Function to reset selected nodes and send the update to Shiny
function isolateSelectedNode() {
  // Check if a node is selected
  if (selectedNodes.size === 0) {
    alert("Please select a node first.");
    return;
  }

  // Create a set to keep track of connected nodes
  const connectedNodes = new Set();

  // Add selected nodes and their connected nodes to the connectedNodes set
  selectedNodes.forEach((selectedNode) => {
    connectedNodes.add(selectedNode);
    nodes.forEach((node) => {
      if (areNodesConnected(selectedNode, node)) {
        connectedNodes.add(node);
      }
    });
  });

  // Filter nodes to keep only the connected nodes
  const newNodes = nodes.filter((node) => connectedNodes.has(node));

  // Filter edges to keep only the connected edges
  const newEdges = links.filter((edge) => {
    return connectedNodes.has(edge.source) && connectedNodes.has(edge.target);
  });

  // Remove unconnected nodes from the simulation
  simulation.nodes(newNodes);

  // Restart the simulation
  simulation.alpha(1).restart();

  // Update the 'node' and 'link' selections
  const updatedNode = node.data(newNodes, (d) => d.id);
  const updatedLink = link.data(newEdges);

  // Apply changes to the 'node' and 'link' elements
  updatedNode.exit().remove();
  updatedLink.exit().remove();

  // Reset the selected nodes set
  selectedNodes.clear();
  // Update the color of pre-selected nodes after the isolation operation
  //updatePreSelectedNodesColor();
  
}

// Function to check if a node is connected to all selected nodes
function isNodeConnectedToAllSelectedNodes(nodeToCheck) {
  for (const selectedNode of selectedNodes) {
    if (!areNodesConnected(nodeToCheck, selectedNode)) {
      return false;
    }
  }
  return true;
}

// Function to check if two nodes are connected by an edge
function areNodesConnected(node1, node2) {
  return links.some(link => (link.source === node1 && link.target === node2) || (link.source === node2 && link.target === node1));
}