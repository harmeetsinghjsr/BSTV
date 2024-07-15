var c = document.getElementById("myCanvas");
var ctx = c.getContext("2d");

const canvas_width = c.width;
const canvas_height = c.height;

let root_x = Math.floor(canvas_width / 2);
let root_y = 50;
let distance_x = 550;
let distance_y = 90;
let depth = 0;
let current_x, current_y, minX, maxX, parentX, parentY;
let reset_nodes = [];

class BST {
  constructor(value, x, y, minWidth, maxWidth) {
    this.value = value;
    this.left = null;
    this.right = null;
    this.x = x;
    this.y = y;
    this.maxWidth = maxWidth;
    this.minWidth = minWidth;
  }
  
  insert(value, x, y) {
    if (value < this.value) {
      if (this.left == null) {
        current_x = this.x;
        current_y = this.y;
        this.left = new BST(value, 0, 0);
        this.left.minWidth = this.minWidth;
        this.left.maxWidth = this.x;
        this.left.x = this.left.minWidth + Math.floor(this.left.maxWidth - this.left.minWidth) / 2;
        this.left.y = current_y + distance_y;
        parentX = current_x;
        parentY = current_y;
        current_x = this.left.x;
        current_y = this.left.y;
      } else {
        this.left.insert(value, x, y);
      }
    } else {
      if (this.right == null) {
        current_x = this.x;
        current_y = this.y;
        this.right = new BST(value, 0, 0);
        this.right.minWidth = this.x;
        this.right.maxWidth = this.maxWidth;
        this.right.x = Math.floor(this.right.maxWidth + this.right.minWidth) / 2;
        this.right.y = current_y + distance_y;
        parentX = current_x;
        parentY = current_y;
        current_x = this.right.x;
        current_y = this.right.y;
      } else {
        this.right.insert(value, x, y);
      }
    }
    return this;
  }

  delete(value) {
    if (value < this.value) {
      if (this.left) {
        this.left = this.left.delete(value);
      }
    } else if (value > this.value) {
      if (this.right) {
        this.right = this.right.delete(value);
      }
    } else {
      if (!this.left && !this.right) {
        return null;
      } else if (!this.left) {
        return this.right;
      } else if (!this.right) {
        return this.left;
      } else {
        const minNode = this.right.findMin();
        this.value = minNode.value;
        this.right = this.right.delete(minNode.value);
      }
    }
    return this;
  }

  findMin() {
    if (!this.left) {
      return this;
    }
    return this.left.findMin();
  }
}

let demo;
let root = true;

function enterNumber() {
  let value = parseInt(document.getElementById("number").value);
  if (root) {
    demo = new BST(value, root_x, root_y, 0, canvas_width);
    root = false;
    draw_circle(root_x, root_y, value, 0, 0);
    current_x = root_x;
    current_y = root_y;
  } else {
    demo.insert(value, current_x, current_y);
    draw_circle(current_x, current_y, value, parentX, parentY);
  }
  reset_Tree_Visual(reset_nodes);
  document.getElementById("number").value = "";
}

function deleteNumber() {
  let value = parseInt(document.getElementById("number").value);
  if (demo) {
    demo = demo.delete(value);
    ctx.clearRect(0, 0, canvas_width, canvas_height); // Clear canvas
    if (demo) {
      redrawTree(demo); // Redraw tree
    }
  }
  document.getElementById("number").value = "";
}

function draw_circle(x, y, value, pX, pY) {
  ctx.beginPath();
  ctx.lineWidth = 5;
  ctx.arc(x, y, 20, 0, 2 * Math.PI, true);
  ctx.fillStyle = "white";
  ctx.fill();
  ctx.stroke();
  ctx.beginPath();
  ctx.font = "15px Arial";
  ctx.fillStyle = "black";
  ctx.fill();
  ctx.textAlign = "center";
  ctx.fillText(value, x, y + 5);
  ctx.stroke();

  if (pX === 0 && pY === 0) return;
  ctx.lineWidth = 5;
  ctx.moveTo(x, y - 20);
  ctx.lineTo(pX - 5, pY + 20); // parent node
  ctx.stroke();
}

function redrawTree(node) {
  if (node) {
    draw_circle(node.x, node.y, node.value, 0, 0);
    if (node.left) {
      draw_circle(node.left.x, node.left.y, node.left.value, node.x, node.y);
      redrawTree(node.left);
    }
    if (node.right) {
      draw_circle(node.right.x, node.right.y, node.right.value, node.x, node.y);
      redrawTree(node.right);
    }
  }
}

function reset_Tree_Visual(reset_nodes) {
  if (reset_nodes.length > 0) {
    for (let i = 0; i < reset_nodes.length; ++i) {
      ctx.beginPath();
      ctx.lineWidth = 5;
      ctx.arc(reset_nodes[i][0], reset_nodes[i][1], 20, 0, 2 * Math.PI, true);
      ctx.fillStyle = "white";
      ctx.fill();
      ctx.stroke();
      ctx.beginPath();
      ctx.font = "15px Arial";
      ctx.fillStyle = "black";
      ctx.fill();
      ctx.textAlign = "center";
      ctx.fillText(reset_nodes[i][2], reset_nodes[i][0], reset_nodes[i][1] + 5);
      ctx.stroke();
    }
    reset_nodes = [];
  }
}

let firstNodeVisual = true;
async function findEle() {
  reset_Tree_Visual(reset_nodes);
  let value = document.getElementById("number").value;
  if (value !== "") {
    if (await contains(demo, parseInt(value))) {
      alert("Found");
    } else {
      alert("Not found!");
    }
  } else {
    alert("Cannot search for a blank value!");
  }
  firstNodeVisual = true;
}

function resetAll() {
  demo = null;
  root = true;
  ctx.clearRect(0, 0, canvas_width, canvas_height);
  document.getElementById("number").value = "";
  document.getElementById("comp").innerHTML = "You can see the current status of the Binary Search here!";
}

function sleep(ms) {
  return new Promise(resolve => setTimeout(resolve, ms));
}

async function contains(demo, value) {
  document.getElementById("comp").innerHTML = "Comparing the value: - " + value + " and " + demo.value;
  if (firstNodeVisual) {
    firstNodeVisual = false;
    ctx.beginPath();
    ctx.lineWidth = 5;
    ctx.arc(demo.x, demo.y, 20, 0, 2 * Math.PI, true);
    ctx.fillStyle = "red";
    ctx.fill();
    ctx.stroke();
    ctx.beginPath();
    ctx.font = "15px Arial";
    ctx.fillStyle = "white";
    ctx.fill();
    ctx.textAlign = "center";
    ctx.fillText(demo.value, demo.x, demo.y + 5);
    ctx.stroke();
    reset_nodes.push([demo.x, demo.y, demo.value]);
  }
  await sleep(1500);
  if (value < demo.value) {
    if (demo.left == null) {
      ctx.beginPath();
      ctx.lineWidth = 5;
      ctx.arc(demo.x, demo.y, 20, 0, 2 * Math.PI, true);
      ctx.fillStyle = "red";
      ctx.fill();
      ctx.stroke();
      ctx.beginPath();
      ctx.font = "15px Arial";
      ctx.fillStyle = "white";
      ctx.fill();
      ctx.textAlign = "center";
      ctx.fillText(demo.value, demo.x, demo.y + 5);
      ctx.stroke();
      reset_nodes.push([demo.x, demo.y, demo.value]);
      document.getElementById("comp").innerHTML = value + " is not found in this tree!";
      return false;
    } else {
      ctx.beginPath();
      ctx.lineWidth = 5;
      ctx.arc(demo.left.x, demo.left.y, 20, 0, 2 * Math.PI, true);
      ctx.fillStyle = "red";
      ctx.fill();
      ctx.stroke();
      ctx.beginPath();
      ctx.font = "15px Arial";
      ctx.fillStyle = "white";
      ctx.fill();
      ctx.textAlign = "center";
      ctx.fillText(demo.left.value, demo.left.x, demo.left.y + 5);
      ctx.stroke();
      reset_nodes.push([demo.left.x, demo.left.y, demo.left.value]);
      return await contains(demo.left, value);
    }
  } else if (value > demo.value) {
    if (demo.right == null) {
      ctx.beginPath();
      ctx.lineWidth = 5;
      ctx.arc(demo.x, demo.y, 20, 0, 2 * Math.PI, true);
      ctx.fillStyle = "red";
      ctx.fill();
      ctx.stroke();
      ctx.beginPath();
      ctx.font = "15px Arial";
      ctx.fillStyle = "white";
      ctx.fill();
      ctx.textAlign = "center";
      ctx.fillText(demo.value, demo.x, demo.y + 5);
      ctx.stroke();
      reset_nodes.push([demo.x, demo.y, demo.value]);
      document.getElementById("comp").innerHTML = value + " is not found in this tree!";
      return false;
    } else {
      ctx.beginPath();
      ctx.lineWidth = 5;
      ctx.arc(demo.right.x, demo.right.y, 20, 0, 2 * Math.PI, true);
      ctx.fillStyle = "red";
      ctx.fill();
      ctx.stroke();
      ctx.beginPath();
      ctx.font = "15px Arial";
      ctx.fillStyle = "white";
      ctx.fill();
      ctx.textAlign = "center";
      ctx.fillText(demo.right.value, demo.right.x, demo.right.y + 5);
      ctx.stroke();
      reset_nodes.push([demo.right.x, demo.right.y, demo.right.value]);
      return await contains(demo.right, value);
    }
  } else {
    ctx.lineWidth = 5;
    ctx.arc(demo.x, demo.y, 20, 0, 2 * Math.PI, true);
    ctx.fillStyle = "green";
    ctx.fill();
    ctx.stroke();
    ctx.beginPath();
    ctx.font = "15px Arial";
    ctx.fillStyle = "white";
    ctx.fill();
    ctx.textAlign = "center";
    ctx.fillText(demo.value, demo.x, demo.y + 5);
    ctx.stroke();
    document.getElementById("comp").innerHTML = value + " is found in this tree!";
    return true;
  }
}