// A rudimentary force layout using Gauss-Seidel.
myforce = function() {
  var force = {},
      alpha,
      friction = .9,
      charge = -30,
      gravity = .1,
      theta = .8,
      nodes = [],
      animated = false,
      charges,
      tick;
  function clamp(x) {
  	//return x;
  	if (x < -0.1) return -0.1;
  	if (x > 0.1) return 0.1;
  	return x;
  }

  function repulse(node) {
    return function(quad, x1, y1, x2, y2) {
      if (quad.point !== node) {
        var dx = quad.cx - node.x,
            dy = quad.cy - node.y,
            dn = 1 / Math.sqrt(dx * dx + dy * dy);

        /* Barnes-Hut criterion. */
        if ((x2 - x1) * dn < theta) {
          var k = quad.charge * dn * dn;
          //console.log(quad.charge);
          node.px -= clamp(dx * k);
          node.py -= clamp(dy * k);
          return true;
        }

        if (quad.point && isFinite(dn)) {
          var k = quad.pointCharge * dn * dn;
          //console.log(quad.pointCharge);
          node.px -= clamp(dx * k);
          node.py -= clamp(dy * k);
        }
      }
      return !quad.charge;
    };
  }

  force.tick = function() {
    // simulated annealing, basically
    if ((alpha *= .99) < .05) {
      return true;
    }

    var n = nodes.length,
        q,
        i, // current index
        o, // current object
        k, // current force
        x, // x-distance
        y; // y-distance

    // compute quadtree center of mass and apply charge forces
    if (charge) {
    
      d3_layout_forceAccumulate(q = d3.geom.quadtree(nodes), alpha, charges);
      i = -1; while (++i < n) {
        if (!(o = nodes[i]).fixed) {
          q.visit(repulse(o));
        }
      }
    }

    // position verlet integration
    i = -1; while (++i < n) {
      o = nodes[i];
      if (o.fixed) {
        o.x = o.px;
        o.y = o.py;
      } else {
        //o.x -= (o.px - (o.px = o.x)) * friction;
        //o.y -= (o.py - (o.py = o.y)) * friction;
        var tx = o.x;
        var ty = o.y;
        o.x -= (o.px - o.x) * friction;
        o.y -= (o.py - o.y) * friction;
        o.px = tx;
        o.py = ty;
      }
    }

    tick(alpha);
  };

  force.nodes = function(x) {
    nodes = x;
    return force;
  };

  force.friction = function(x) {
    friction = x;
    return force;
  };

  force.charge = function(x) {
    charge = typeof x === "function" ? x : +x;
    return force;
  };

  force.theta = function(x) {
    theta = x;
    return force;
  };
  
  force.animated = function(x) {
  	animated = x;
  	return force;
  }
  
  force.onTick = function(x) {
  	tick = x;
  	return force;
  }

  force.alpha = function(x) {
    if (alpha) { // if we're already running
      if (x > 0) alpha = x; // we might keep it hot
      else alpha = 0; // or, next tick will dispatch "end"
    } else if (x > 0) { // otherwise, fire it up!
      alpha = x;
      if (animated) {
      	d3.timer(force.tick);
      } else {
      	while (!force.tick()) {
      	}
      }
    }

    return force;
  };

  force.start = function() {
    var i,
        j,
        n = nodes.length,
        o;

    for (i = 0; i < n; ++i) {
      (o = nodes[i]).index = i;
    }

    for (i = 0; i < n; ++i) {
      o = nodes[i];
      //if (isNaN(o.x)) o.x = position("x", w);
      //if (isNaN(o.y)) o.y = position("y", h);
      if (isNaN(o.px)) o.px = o.x;
      if (isNaN(o.py)) o.py = o.y;
    }

    charges = [];
    if (typeof charge === "function") {
      for (i = 0; i < n; ++i) {
        charges[i] = +charge.call(this, nodes[i], i);
      }
    } else {
      for (i = 0; i < n; ++i) {
        charges[i] = charge;
      }
    }

    return force.resume();
  };

  force.resume = function() {
    return force.alpha(.1);
  };

  force.stop = function() {
    return force.alpha(0);
  };

  return force;
};

var randomSeed = 0;
function d3_layout_forceAccumulate(quad, alpha, charges) {
  var random = function () {
  	randomSeed = randomSeed ? 0 : 1;
  	return randomSeed ? 0.01 : -0.01;
  };
  var cx = 0,
      cy = 0;
  quad.charge = 0;
  if (!quad.leaf) {
    var nodes = quad.nodes,
        n = nodes.length,
        i = -1,
        c;
    while (++i < n) {
      c = nodes[i];
      if (c == null) continue;
      d3_layout_forceAccumulate(c, alpha, charges);
      quad.charge += c.charge;
      cx += c.charge * c.cx;
      cy += c.charge * c.cy;
    }
  }
  if (quad.point) {
    // jitter internal nodes that are coincident
    if (!quad.leaf) {
      //quad.point.x += Math.random() - .5;
      //quad.point.y += Math.random() - .5;
      quad.point.x += random();
      quad.point.y += random();
    }
    var k = alpha * charges[quad.point.index];
    quad.charge += quad.pointCharge = k;
    cx += k * quad.point.x;
    cy += k * quad.point.y;
  }
  quad.cx = cx / quad.charge;
  quad.cy = cy / quad.charge;
}
