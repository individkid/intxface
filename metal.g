#include <metal_matrix>

struct Triangle {
   uint4 vtx; // points of triangle
   uint num; // plane of points
   uint tex; // texture of triangle
   uint pol; // polytope triangle is in
   uint pad;
};
struct Numeric {
   float4 vec; // distances above basis
   uint bas; // basis selector
   uint pad0, pad1, pad2;
};
struct Vertex {
   float4 vec; // intersection of planes
   uint ref; // backreference to planes
   uint pad0, pad1, pad2;
};
struct Pierce {
   float4 fix;
   float4 nml;
   uint vld;
   uint idx;
   uint pad0, pad1;
};
struct Vector {
   float4 vec;
};
struct Matrix {
   metal::float4x4 mat;
};
struct Basis {
   float4 bas[3][3];
};
struct Uniform {
   uint all; // which subject to use
   uint one; // which element to use
   uint lon; // horizontal axis of cursor
   uint lat; // vertical axis of cursor
   uint pro; // which subject for projection
   uint idx; // which plane to apply one matrix to
   uint use; // which basis to use
   uint siz; // how many swarm points there are
};

struct Expand {
   float3 point[3];
};
struct Triple {
   Expand plane[3];
};
struct Qualify {
   float quality;
   float3 point;
};
struct Quality {
   float quality;
   float3 left;
   float3 right;
};

Expand expand(Numeric plane, const device Basis *basis)
{
   Expand result;
   for (uint i = 0; i < 3; i++) {
      result.point[i] = basis->bas[plane.bas][i].xyz;
      result.point[i][plane.bas] = plane.vec[i];}
   return result;
}
float3 barrycentric(Expand plane, uint versor, float3 point)
{
   uint3 sub;
   for (uint i = 0; i < 3; i++) sub[i] = (versor+i+1)%3;
   float x = point[sub[0]];
   float y = point[sub[1]];
   float x1 = plane.point[0][sub[0]];
   float x2 = plane.point[1][sub[0]];
   float x3 = plane.point[2][sub[0]];
   float y1 = plane.point[0][sub[1]];
   float y2 = plane.point[1][sub[1]];
   float y3 = plane.point[2][sub[1]];
   float3 lambda;
   lambda.x = ((y2-y3)*(x-x3)+(x3-x2)*(y-y3))/((y2-y3)*(x1-x3)+(x3-x2)*(y1-y3));
   lambda.y = ((y3-y1)*(x-x3)+(x1-x3)*(y-y3))/((y2-y3)*(x1-x3)+(x3-x2)*(y1-y3));
   lambda.z = 1.0-lambda.x-lambda.y;
   return lambda;
}
float3 project(Expand plane, uint versor, float3 point)
{
   float3 lambda = barrycentric(plane,versor,point);
   point[versor] = 0.0;
   for (uint i = 0; i < 3; i++) point[versor] += lambda[i]*plane.point[i][versor];
   return point;
}
float extreme(Expand plane, uint versor)
{
   float best = -1.0;
   for (uint i = 0; i < 3; i++) {
      int other[2];
      for (uint j = i+1; j < i+3; j++)
         other[j-i-1] = j%3;
      float difference = metal::abs(plane.point[other[0]][versor]-plane.point[other[1]][versor]);
      if (difference > best)
         best = difference;}
   return best;
}
uint squashed(Expand plane)
{
   uint best = 0;
   float difference = INFINITY;
   for (uint i = 0; i < 3; i++) {
      float temp = extreme(plane,i);
      if (temp < difference) {
         difference = temp;
         best = i;}}
   return best;
}
bool opposite(Expand plane, float3 left, float3 right)
{
   uint versor = squashed(plane);
   bool temp0 = (left[versor]>project(plane,versor,left)[versor]);
   bool temp1 = (right[versor]>project(plane,versor,right)[versor]);
   return temp0^temp1;
}
Qualify intrasect(Expand plane, float3 left, float3 right)
{ // intersection of line through points with plane
   Qualify qualify;
   uint versor = squashed(plane);
   float3 origin = project(plane,versor,left);
   float3 point = project(plane,versor,right);
   float numerator = origin[versor]-left[versor];
   float denominator = numerator+(right[versor]-point[versor]);
   float numer = metal::abs(numerator);
   float denom = metal::abs(denominator);
   if (denom < 1.0 && INFINITY*denom < numer) {
      qualify.quality = INFINITY;
      qualify.point = float3(INFINITY,INFINITY,INFINITY);
   } else {
      qualify.quality = numer/denom;
      qualify.point = left+(right-left)*numerator/denominator;
   }
   return qualify;
}
Quality inteasect(Expand left, Expand right)
{ // two points on intersection between planes
   Quality best;
   best.quality = INFINITY;
   for (uint i = 0; i < 3; i++) {
      int other[2];
      for (uint j = i+1; j < i+3; j++)
         other[j-i-1] = j%3;
      Qualify temp0 = intrasect(left,right.point[other[0]],right.point[i]);
      Qualify temp1 = intrasect(left,right.point[other[1]],right.point[i]);
      float worst;
      if (temp0.quality < temp1.quality)
         worst = temp1.quality; else worst = temp0.quality;
      if (worst < best.quality) {
         best.quality = worst;
         best.left = temp0.point;
         best.right = temp1.point;}}
   return best;
}
Quality intrrsect(Expand left, Expand right)
{ // best point pair on intersection between planes
   Quality fore = inteasect(left,right);
   Quality back = inteasect(right,left);
   if (fore.quality < back.quality)
      return fore;
   return back;
}
Qualify intersect(Triple point)
{ // best intersection between three planes
   Qualify best;
   best.quality = INFINITY;
   for (uint i = 0; i < 3; i++) {
      int other[2];
      for (uint j = i+1; j < i+3; j++)
         other[j-i-1] = j%3;
      Quality intrr = intrrsect(point.plane[other[0]],point.plane[other[1]]);
      Qualify intra = intrasect(point.plane[i],intrr.left,intrr.right);
      if (intra.quality < intrr.quality)
         intra.quality = intrr.quality;
      if (intra.quality < best.quality)
         best = intra;}
   return best;
}
float3 normal(Expand plane)
{ // normal to plane through three points
   return metal::normalize( metal::cross( plane.point[1] - plane.point[0] , plane.point[2] - plane.point[0] ) );
}
float3 average3(Expand plane)
{ // average between three points
   float3 total = float3(0.0,0.0,0.0);
   for (uint i = 0; i < 3; i++)
      total += plane.point[i];
   return total/3.0;
}
float3 normal3(Triple point)
{ // average normal to three planes
   Expand plane;
   for (uint i = 0; i < 3; i++) plane.point[i] = normal(point.plane[i]);
   return average3(plane);
}
Expand transform(Expand plane, metal::float4x4 matrix)
{ // three points transformed
   Expand result;
   for (uint i = 0; i < 3; i++) {
      result.point[i] = plane.point[i];
      float4 affine = float4(result.point[i],1.0);
      result.point[i] = metal::normalize(matrix*affine).xyz;}
   return result;
}
struct VertexOutput {
   float4 position [[position]];
   float3 normal;
   float4 color;
   float2 coord;
};
vertex VertexOutput vertex_render(
   const device Triangle *corner [[buffer(0)]],
   const device Numeric *plane [[buffer(1)]],
   const device Vertex *point [[buffer(2)]],
   const device Matrix *subject [[buffer(3)]],
   const device Matrix *object [[buffer(4)]],
   const device Matrix *element [[buffer(5)]],
   const device Basis *basis [[buffer(6)]],
   const device Uniform *state [[buffer(7)]],
   uint id [[vertex_id]])
{
   VertexOutput out;
   return out; // TODO
}
fragment half4 fragment_render(
   VertexOutput in [[stage_in]])
{
   return half4(in.color);
}
kernel void kernel_pierce(
   const device Triangle *corner [[buffer(0)]],
   const device Numeric *plane [[buffer(1)]],
   const device Vertex *point [[buffer(2)]],
   const device Matrix *subject [[buffer(3)]],
   const device Matrix *object [[buffer(4)]],
   const device Matrix *element [[buffer(5)]],
   const device Basis *basis [[buffer(6)]],
   const device Uniform *state [[buffer(7)]],
   uint id [[thread_position_in_grid]],
   device Pierce *pierce [[buffer(8)]])
{
   Expand expand;
   for (uint i = 0; i < 3; i++) expand.point[i] = point[corner[id].vtx[i]].vec.xyz;
   pierce[id].fix = float4(project(expand,2,float3(state->lon,state->lat,0.0)),0.0);
   pierce[id].nml = float4(normal(expand),0.0);
   pierce[id].vld = 1;
   pierce[id].idx = id;
   // TODO
}
