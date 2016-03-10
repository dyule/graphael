(function () {
  var width,
      height,
      color,
      force,
      svg;

  function initialize() {
    width = 960,
    height = 500;

    color = d3.scale.category20();

    force = d3.layout.force()
      .charge(-320)
      .linkDistance(100)
      .size([width, height]);

    svg = d3.select("svg")
      .attr("width", width)
      .attr("height", height);

      document.getElementById('do_query').addEventListener("click", function () {
        var query_text = document.getElementById('query').value;
        do_query(query_text);
      });

      document.getElementById('query').addEventListener("keydown", function(e) {
        if (e.keyCode == 13) {
          document.getElementById('do_query').click();
        }
      });
  }

  function do_query(query) {
    document.getElementById('error').innerHTML = "";
    d3.json("/query?q="+encodeURIComponent(query), function(error, result) {
      if (error) {
        var error_response = JSON.parse(error.responseText).message;
        document.getElementById('error').innerHTML = error_response;
        return;
      }
      var graph = result.graph;

      force
          .nodes(graph.nodes)
          .links(graph.links)
          .start();

      svg.selectAll('*').remove();
      var link = svg.selectAll(".link")
          .data(graph.links)
        .enter().append("line")
          .attr("class", "link");

      var node = svg.selectAll(".node")
          .data(graph.nodes)
        .enter().append("circle")
          .attr("class", "node")
          .attr("r", 20)
          .style("fill", function(d) { return color(1); })
          .call(force.drag);

      node.append("title")
          .text(function(d) { return d.props.name.value; });


      var text = svg.selectAll(".text")
      .data(graph.nodes)
      .enter().append("text")
      .attr("class", "text")
      .text(function(d) {return d.props.name.value});
    force.on("tick", function() {
      link.attr("x1", function(d) { return d.source.x; })
          .attr("y1", function(d) { return d.source.y; })
          .attr("x2", function(d) { return d.target.x; })
          .attr("y2", function(d) { return d.target.y; });

      node.attr("cx", function(d) { return d.x; })
          .attr("cy", function(d) { return d.y; });

      text.attr("x", function(d) { return d.x; })
          .attr("y", function(d) { return d.y; });
    });
  });
}

document.addEventListener("DOMContentLoaded", initialize);

})()
