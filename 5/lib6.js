
let fragmentShaderHeader = [''                      // WHATEVER CODE WE WANT TO PREDEFINE FOR FRAGMENT SHADERS
,'   precision highp float;'
,'float noise(vec3 v) {'
,'   vec4 r[2];'
,'   const mat4 E = mat4(0.,0.,0.,0., 0.,.5,.5,0., .5,0.,.5,0., .5,.5,0.,0.);'
,'   for (int j = 0 ; j < 2 ; j++)'
,'   for (int i = 0 ; i < 4 ; i++) {'
,'      vec3 p = .60*v + E[i].xyz, C = floor(p), P = p - C-.5, A = abs(P), D;'
,'      C += mod(C.x+C.y+C.z+float(j),2.) * step(max(A.yzx,A.zxy),A)*sign(P);'
,'      D  = 314.1*sin(59.2*float(i+4*j) + 65.3*C + 58.9*C.yzx + 79.3*C.zxy);'
,'      r[j][i] = dot(P=p-C-.5,fract(D)-.5) * pow(max(0.,1.-2.*dot(P,P)),4.);'
,'   }'
,'   return 6.50 * (r[0].x+r[0].y+r[0].z+r[0].w+r[1].x+r[1].y+r[1].z+r[1].w);'
,'}'
].join('\n');

let nfsh = fragmentShaderHeader.split('\n').length;         // # LINES OF CODE IN fragmentShaderHeader

let isFirefox = navigator.userAgent.indexOf('Firefox') > 0; // IS THIS THE FIREFOX BROWSER?
let errorMsg = '';

let gl;
let stride = 6;                                             // NUMBER OF VALUES PER VERTEX.

/**
We can create a shape as a 2D rectangular mesh of vertices by sending a single triangle strip 
for first the first row, then the second, then the third, etc. 
In order to avoid problems of spurious triangles as between the end of one row and the beginning of the next, 
we add two extra vertices, to create degenerate triangles that won't show up in the final rendering.

First argument is a function that takes (u,v) as arguments 
and returns the data for a single vertex:
*/
let createTriangleMesh = (uvToVertex, nCols, nRows) => {
   let mesh = [];
   let appendVertex = p => {
      for (let n = 0 ; n < p.length ; n++)
         mesh.push(p[n]);
   }
   for (let row = 0 ; row < nRows ; row++) {
      let v0 =  row    / nRows,
          v1 = (row+1) / nRows;
      for (let col = 0 ; col <= nCols ; col++) {
         let u = col / nCols;
         appendVertex(uvToVertex(u, v0));
         appendVertex(uvToVertex(u, v1));
      }
      appendVertex(uvToVertex(1, v1));
      appendVertex(uvToVertex(0, v1));
   }
   return mesh;
}
/**
In order to implement any particular primitive as a triangle strip, 
you need to convert any parametric value of (u,v), where 0 ≤ u ≤ 1 and 0 ≤ v ≤ 1, into six numbers.
Those six numbers represent a surface point P and a surface normal N.
*/

//Sphere
let uvToSphere = (u,v) => {
   let theta = 2 * Math.PI * u;
   let phi   = Math.PI * v - Math.PI / 2;
   let x = Math.cos(theta) * Math.cos(phi),
       y = Math.sin(theta) * Math.cos(phi),
       z = Math.sin(phi);
   return [ x,y,z, x,y,z ]; // RETURN POSITION (P) AND NORMAL (N)
}

//Sqaure
/**
P = [ 2u-1 , 2v-1 , 0 ]
N = [ 0 , 0 , 1 ]
*/
let uvToSquare = (u,v) => {
   let P = [2*u-1, 2*v-1, 0];
   let N = [0,0,1];
   
   return [ P[0],P[1],P[2], N[0],N[1],N[2] ]; // RETURN POSITION (P) AND NORMAL (N)
}

//Cyclindrical Tube
/**
let θ = 2 π u
P = [ cos(θ) , sin(θ) , 2v-1 ]
N = [ cos(θ) , sin(θ) , 0 ]
*/
let uvToCylinder = (u,v) => {
   let theta = 2 * Math.PI * u;
   let P = [Math.cos(theta), Math.sin(theta), 2*v-1];
   let N = [Math.cos(theta), Math.sin(theta), 0];
   return [ P[0],P[1],P[2], N[0],N[1],N[2] ]; // RETURN POSITION (P) AND NORMAL (N)
}

//Torus
/**
let θ = 2 π u
let φ = 2 π v
P = [ cos(θ) (1 + r cos(φ) , sin(θ) (1 + r cos(φ) , r sin(φ) ]
N = [ cos(θ) cos(φ) , sin(θ) cos(φ) , sin(φ) ]

(cosTheta, sinTheta, 0) //builder function
P (cosTheta(1 + rcosPhi), sinTheta, (1 + rcosPhi), rsinPhi); //equation for torus

*/

let r0 = 0.3;
let uvToTorus = (u,v) => {
   let theta = 2 * Math.PI * u;
   let phi   = 2 * Math.PI * v;

   let P = [ 
   	Math.cos(theta) * (1 + r0*Math.cos(phi)), 
    Math.sin(theta) * (1 + r0*Math.cos(phi)), 
    r0*Math.sin(phi)
    ];

   let N = [
   	Math.cos(theta)*Math.cos(phi),
   	Math.sin(theta)*Math.sin(phi),
   	Math.sin(phi)
   ];

   return [ P[0],P[1],P[2], N[0],N[1],N[2] ];
   
}

/***********
*/
//30 columns and 15 rows, and with position (x,y,z) and normal (nx,ny,nz) at each vertex
let sphere = createTriangleMesh(uvToSphere, 30, 15);

let square = createTriangleMesh(uvToSquare, 30, 15);
let square2 = createTriangleMesh(uvToSquare, 30, 15);

let cylinder = createTriangleMesh(uvToCylinder, 30, 15);
let cylinder2 = createTriangleMesh(uvToCylinder, 30, 15);
let cylinder3 = createTriangleMesh(uvToCylinder, 30, 15);
let cylinder4 = createTriangleMesh(uvToCylinder, 30, 15);
let cylinder5 = createTriangleMesh(uvToCylinder, 30, 15);
let cylinder6 = createTriangleMesh(uvToCylinder, 30, 15);

let torus = createTriangleMesh(uvToTorus, 30, 15);
let torus2 = createTriangleMesh(uvToTorus, 30, 15);
let torus3 = createTriangleMesh(uvToTorus, 30, 15);

/***********
*/

function gl_start(canvas, vertexShader, fragmentShader) {           // START WEBGL RUNNING IN A CANVAS

   setTimeout(function() {
      try { 
         canvas.gl = canvas.getContext('experimental-webgl');              // Make sure WebGl is supported.
      } catch (e) { throw 'Sorry, your browser does not support WebGL.'; }

      canvas.setShaders = function(vertexShader, fragmentShader) {         // Add the vertex and fragment shaders:

         gl = this.gl, program = gl.createProgram();                        // Create the WebGL program.

         function addshader(type, src) {                                        // Create and attach a WebGL shader.
            function spacer(color, width, height) {
               return '<table bgcolor=' + color +
                            ' width='   + width +
                            ' height='  + height + '><tr><td>&nbsp;</td></tr></table>';
            }
            errorMessage.innerHTML = '<br>';
            errorMarker.innerHTML = spacer('black', 1, 1) + '<font size=5 color=black>\u25B6</font>';
            let shader = gl.createShader(type);
            gl.shaderSource(shader, src);
            gl.compileShader(shader);
            if (! gl.getShaderParameter(shader, gl.COMPILE_STATUS)) {
               let msg = gl.getShaderInfoLog(shader);
               console.log('Cannot compile shader:\n\n' + msg);

               let a = msg.substring(6, msg.length);
               if (a.substring(0, 3) == ' 0:') {
                  a = a.substring(3, a.length);
                  let line = parseInt(a) - nfsh;
                  let nPixels = isFirefox ? 17 * line - 10 : 18 * line - 1;
                  errorMarker.innerHTML = spacer('black', 1, nPixels) + '<font size=5>\u25B6</font>';
               }

               let j = a.indexOf(':');
               a = a.substring(j+2, a.length);
               if ((j = a.indexOf('\n')) > 0)
                  a = a.substring(0, j);
               errorMessage.innerHTML = a;
            }
            gl.attachShader(program, shader);
         };

         addshader(gl.VERTEX_SHADER  , vertexShader  );                         // Add the vertex and fragment shaders.
         addshader(gl.FRAGMENT_SHADER, fragmentShaderHeader + fragmentShader);

         gl.linkProgram(program);                                               // Link the program, report any errors.
         if (! gl.getProgramParameter(program, gl.LINK_STATUS))
            console.log('Could not link the shader program!');
         gl.useProgram(program);
         gl.program = program;

         gl.enable(gl.DEPTH_TEST);                                              // Set up WebGL to render the
         gl.depthFunc(gl.GEQUAL);                                               // nearest object at each pixel.
         gl.clearDepth(-1.0);

         gl.bindBuffer(gl.ARRAY_BUFFER, gl.createBuffer());                     // Create a square as a triangle strip

         /*
         Since we now have multiple attributes -- poth position and normal -- 
         to send down the the vertex shader, in our Javascript we now need to 
         map out the structure of each vertex in the vertex buffer more clearly:
         */
         let aPos = gl.getAttribLocation(program, 'aPos');                      // Set aPos attribute for each vertex.
         let aNor = gl.getAttribLocation(program, 'aNor');                      // Set aNor attribute for each vertex.
         gl.enableVertexAttribArray(aPos);
         gl.enableVertexAttribArray(aNor);										
	     let bpe = Float32Array.BYTES_PER_ELEMENT;								// Bytes per floating point number
         gl.vertexAttribPointer(aPos, 3, gl.FLOAT, false, stride * bpe, 0      );// aPos is 0,1,2 within the 6 slots.
         gl.vertexAttribPointer(aNor, 3, gl.FLOAT, false, stride * bpe, 3 * bpe);// aNor is 3,4,5 within the 6 slots.
      }

      canvas.setShaders(vertexShader, fragmentShader);                     // Initialize everything,

      setInterval(function() {                                             // Start the animation loop.
         gl = canvas.gl;
	 gl.clear(gl.DEPTH_BUFFER_BIT);
         animate();
      }, 30);

   }, 100); // Wait 100 milliseconds after page has loaded before starting WebGL.
}

function invert(src) {
  let dst = [], det = 0, cofactor = (c, r) => {
     let s = (i, j) => src[c+i & 3 | (r+j & 3) << 2];
     return (c+r & 1 ? -1 : 1) * ( (s(1,1) * (s(2,2) * s(3,3) - s(3,2) * s(2,3)))
                                 - (s(2,1) * (s(1,2) * s(3,3) - s(3,2) * s(1,3)))
                                 + (s(3,1) * (s(1,2) * s(2,3) - s(2,2) * s(1,3))) );
  }
  for (let n = 0 ; n < 16 ; n++) dst.push(cofactor(n >> 2, n & 3));
  for (let n = 0 ; n <  4 ; n++) det += src[n] * dst[n << 2];
  for (let n = 0 ; n < 16 ; n++) dst[n] /= det;
  return dst;
}

function animate() { }                   // animate() callback function can be redefined in index.html.

function setUniform(type, name, a, b, c, d, e, f) {
   let loc = gl.getUniformLocation(gl.program, name);
   (gl['uniform' + type])(loc, a, b, c, d, e, f);
}

