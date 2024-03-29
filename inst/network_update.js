// Constants object for viz, all can be overwritten if passed a different value
// with the options argument of r2d3
const C = Object.assign(
  {
    padding: 20,
    tooltip_offset: 13,
    tooltip_height: 60,
    margin: {right: 20, left: 20, top: 20, bottom: 5},
    case_radius: 2,
    code_radius_mult: 3,
    case_opacity: 1,
    edge_color: '#aaa',
    progress_bar_height: 20,
    progress_bar_color: 'orangered',
    msg_loc: 'shiny_server',
    viz_type: 'bipartite',
    update_freq: 5, // How often do we send back layout simulation progress?
  },
  options);


function setup_webworker(C){
  let worker;

  // Wrap up function into a fake script to call
  const worker_url = URL.createObjectURL(
    new Blob(['('+sim_webworker+`)(${C.update_freq})`])
  );

  const send_new_job = function(network_data, callbacks){

    if(worker){
      // If we already have a webworker running make sure to
      // terminate it so we don't have multiple going at the
      // same time.
      worker.terminate();
    }

    const {on_progress_report, on_finish, on_layout_data} = callbacks;

    // Initialize the worker
    worker = new Worker(worker_url);

    // Send worker the data we are working with
    worker.postMessage(network_data);


    // Control what is done when a message is received from the webworker
    worker.onmessage = function(event) {
      switch (event.data.type) {
        case "progress_report": return on_progress_report(event.data.progress);
        case "layout_data": return on_layout_data(event.data);
        case "end": return on_finish(event.data);
      }
    };
  };

  return send_new_job;
}


// Function to entirely setup network viz.
// Exposes methods for adding new data and updating the size
function setup_network_viz(dom_elements, on_node_click){

  let layout_data = null,
      been_sized = false,
      current_zoom = null,
      scales = null,
      patient_patterns = null,
      nodes_to_highlight = [];

  const X = d3.scaleLinear();
  const Y = d3.scaleLinear();

  const new_data = function(data){

    const nodes = data.nodes || data.vertices;
    const links = data.links || data.edges;

    // Remove old network nodes
    dom_elements.svg.selectAll('circle').remove();

    // Update scale domains
    X.domain(d3.extent(nodes, d => d.x));
    Y.domain(d3.extent(nodes, d => d.y));

    // Update function scope's copy of data
    layout_data = {nodes, links};

    if(been_sized){
      draw();
    }
  };

  const resize = function({width, height, margin}){

    // Update scale ranges
    X.range([margin.left, width - margin.right]);
    Y.range([height - margin.bottom, margin.top]);

    // Let scope know we have set a size for the viz.
    been_sized = true;

    if(layout_data){
      draw();
    }
  };

  const update_scales = function(){
     // Update scales with the zoom if we have any
    scales = {
      X: current_zoom ? current_zoom.rescaleX(X) : X,
      Y: current_zoom ? current_zoom.rescaleY(Y) : Y,
    };
  };

  const draw = function(){
    // Update scales with the zoom if we have any
    update_scales();

    // Draw svg nodes of network
    draw_svg_nodes(layout_data, scales, dom_elements, C, on_node_click, d => highlight([d.name]));
    draw_canvas_portion(layout_data, scales, dom_elements, C, nodes_to_highlight);
  };

  const new_patterns = function(patterns){
    patient_patterns = patterns;
  };

  const highlight = function(codes_to_highlight){
    if(layout_data){

      const single_code = typeof codes_to_highlight === 'string';

      // Find the patient nodes who have the pattern we want to highlight
      const to_highlight = patient_patterns
        .filter(d => arrays_equal(d.pattern, single_code ? [codes_to_highlight] : codes_to_highlight))
        .map(d => d.name);

      // Make sure scales are update width current zoom
      update_scales();

      // Update the canvas to highlight these nodes
      draw_canvas_portion(layout_data, scales, dom_elements, C, to_highlight);
    }
  };

  dom_elements.svg.call(
    d3.zoom()
    .scaleExtent([0.5, 5])
    .on("zoom", () => {
      // Record the zoom event to function scope.
      current_zoom = d3.event.transform;

      // Redraw network with this zoom scale
      draw();
    })
  );

  return {new_data, resize, new_patterns, highlight};
};


//------------------------------------------------------------
// On initialization.
//------------------------------------------------------------
// This code gets run once and sets up the basic
// neccesities for the visualization.

// Setup all the dom elements for the viz. This includes
// the svg, canvas, context, tooltip, and message buttons.
const dom_elements = setup_dom_elements(
  div, C,
  // Function that is called when a message button is pressed. Passed the type of message
  function(type){
    send_to_shiny(type, selected_codes, C);
  });

// Setup the progress bar for network simulation progress
const progress_meter = setup_progress_meter(dom_elements.svg, C);

// Setup the actual network viz itself.
const network_viz = setup_network_viz(dom_elements, on_node_click);
network_viz.new_data(data);

const webworker = setup_webworker(C);

// Holds the currently selected codes.
let selected_codes = [],
    viz = {
      data: {},
      width,
      height,
      options,
      patient_patterns: null,
    };




//------------------------------------------------------------
// On Render
//------------------------------------------------------------
// This is code that runs whenever new data is received by the
// visualization.
r2d3.onRender(function(data, div, width, height, options){
  draw_viz(data, div, width, height, options)

});

function draw_viz(data, div, width, height, options){
  
  const new_data = is_new_data(viz.data, data);

  if(new_data){
     // Update the global viz info object
    viz.data = data;
  } else {
    network_viz.highlight(options.highlighted_pattern);
  }

  viz.options = options;

  if(new_data){
    // Build a patient pattern array and send to network viz
    viz.patient_patterns = build_patient_patterns(viz.data);
    network_viz.new_patterns(viz.patient_patterns);

    // Prepare data based upon the desired format from options
    const data_for_viz = C.viz_type === 'bipartite' ?
      fix_nodes_to_line(sanitize_data(viz.data), C):
      sanitize_data(viz.data);

    // Make sure viz is correct size.
    size_viz(viz.width, viz.height);

    // Launch webworker to calculate layout and kickoff network viz after finishing
    webworker(
      data_for_viz,
      {
        on_progress_report: progress_meter.update,
        on_layout_data: (d) => {
          network_viz.new_data(d);
        },
        on_finish: () => {
          progress_meter.hide();
        },
      }
    );
  }
}


// Tell r2d3 what to do when we resize the viz
r2d3.onResize((width, height) => {
  // Update the global viz info object
  viz.width = width;
  viz.height = height;

  size_viz(viz.width, viz.height);
});


function size_viz(width, height){
  // Adjust size of svg and canvas elements
  const sizes = setup_sizes(width, height, C);
  dom_elements.resize(sizes);
  progress_meter.resize(sizes);
  network_viz.resize(sizes);
}
