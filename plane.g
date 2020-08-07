/*
*    plane.g
*
*    This program is free software: you can redistribute it and/or modify
*    it under the terms of the GNU General Public License as published by
*    the Free Software Foundation, either version 3 of the License, or
*    (at your option) any later version.
*
*    This program is distributed in the hope that it will be useful,
*    but WITHOUT ANY WARRANTY; without even the implied warranty of
*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*    GNU General Public License for more details.
*
*    You should have received a copy of the GNU General Public License
*    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "plane.h"

Expand expand(Facet plane, const device State *state)
{
   Expand result;
   for (uint i = 0; i < 3; i++) {
      result.point[i] = state->basis.plane[plane.versor].point[i];
      result.point[i][plane.versor] = plane.plane[i];}
   return result;
}
float3 barrycentric(Expand plane, uint versor, float3 point)
{
   uint3 sub;
   for (uint i = 0; i < 3; i++) sub[i] = (versor+i+1)%3;
   float x = point[sub[0]];
   float y = point[sub[1]];
   float x1 = plane.point[1][sub[0]];
   float x2 = plane.point[2][sub[0]];
   float x3 = plane.point[3][sub[0]];
   float y1 = plane.point[1][sub[1]];
   float y2 = plane.point[2][sub[1]];
   float y3 = plane.point[3][sub[1]];
   float3 lambda;
   lambda.x = ((y2-y3)*(x-x3)+(x3-x2)*(y-y3))/((y2-y3)*(x1-x3)+(x3-x2)*(y1-y3));
   lambda.y = ((y3-y1)*(x-x3)+(x1-x3)*(y-y3))/((y2-y3)*(x1-x3)+(x3-x2)*(y1-y3));
   lambda.z = 1-lambda.x-lambda.y;
   return lambda;
}
float cartesian(Expand plane, uint versor, float3 lambda)
{
   float value = 0.0;
   for (uint i = 0; i < 3; i++) value += lambda[i]*plane.point[i][versor];
   return value;
}
float3 project(Expand plane, uint versor, float3 point)
{
   point[versor] = cartesian(plane,versor,barrycentric(plane,versor,point));
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
{
   Qualify qualify;
   uint versor = squashed(plane);
   float3 origin = project(plane,versor,left);
   float3 point = project(plane,versor,right);
   float numerator = origin.z-left.z;
   float denominator = numerator-point.z+right.z;
   float numer = metal::abs(numerator);
   float denom = metal::abs(denominator);
   if (denom < 1.0 && INFINITY*denom < numer) {
      qualify.quality = INFINITY;
      qualify.point = float3(INFINITY,INFINITY,INFINITY);
   } else {
      qualify.quality = numer/denom;
      qualify.point = right*numerator/denominator;
   }
   return qualify;
}
Quality inteasect(Expand left, Expand right)
{
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
{
   Quality fore = inteasect(left,right);
   Quality back = inteasect(right,left);
   if (fore.quality < back.quality)
      return fore;
   return back;
}
float3 intersect(Triple point)
{
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
   return best.point;
}
float3 normal(Expand plane)
{
   uint versor = squashed(plane);
   int other[2];
   for (uint i = versor+1; i < versor+3; i++)
      other[i-versor-1] = i%3;
   float x = plane.point[other[0]].x;
   float y = plane.point[other[0]].y;
   float z = plane.point[other[0]].z;
   metal::float3x3 u;
   u[0][0] =  0; u[1][0] = -z; u[2][0] =  y;
   u[0][1] =  z; u[1][1] =  0; u[2][1] = -x;
   u[0][2] = -y; u[1][2] =  x; u[2][2] =  0;
   float3 cross = u*plane.point[other[1]];
   float magnitude = metal::sqrt(metal::dot(cross,cross));
   return cross/magnitude;
}
float3 average3(Expand plane)
{
   float3 total = float3(0.0,0.0,0.0);
   for (uint i = 0; i < 3; i++)
      total += plane.point[i];
   return total/3.0;
}
float3 normal3(Triple point)
{
   Expand plane;
   for (uint i = 0; i < 3; i++) plane.point[i] = normal(point.plane[i]);
   return average3(plane);
}
Expand transform(Expand plane, metal::float4x4 matrix)
{
   Expand result;
   for (uint i = 0; i < 3; i++) {
      result.point[i] = plane.point[i];
      float4 affine = float4(result.point[i],1.0);
      result.point[i] = (matrix*affine).xyz;}
   return result;
}
uint copoint(
   uint3 map,
   const device Facet *plane,
   const device State *state)
{
   uint result = 0;
   for (uint i = 0; i < 3; i++) {
      uint index = map[i];
      if (plane[index].tag == state->tag) result = index;}
   return result;
}
uint coplane(
   uint3 map,
   uint ident)
{
   uint result = 0;
   for (uint i = 0; i < 3; i++)
      if (map[i] == ident) result = i;
   return result;
}
Triple explode(
   uint3 map,
   const device Facet *plane,
   const device State *state)
{
   Triple result;
   for (uint i = 0; i < 3; i++) {
      uint index = map[i];
      result.plane[i] = expand(plane[index],state);
      if (state->hand == index) for (uint j = 0; j < 3; j++) {
         float4 affine = float4(result.plane[i].point[j],1.0);
         result.plane[i].point[j] = (state->feature*affine).xyz;}}
   return result;
}
float4 convert(
   uint index,
   Triple triple,
   const device Facet *plane,
   const device Object *object,
   const device State *state)
{
   float4 position = float4(intersect(triple),1.0);
   position = object[plane[index].poly].object*position;
   position = state->subject*position;
   // TODO add perspective from arrow
   return position;
}
Expand prepare(
   uint index,
   const device Facet *plane,
   const device Object *object,
   const device State *state)
{
   Expand face = expand(plane[index],state);
   if (state->hand == index) face = transform(face,state->feature);
   transform(face,object[plane[index].poly].object);
   transform(face,state->subject);
   // TODO add perspective from arrow
   return face;
}
// MAIN
struct VertexOutput {
   float4 position [[position]];
   float3 normal;
   float4 color;
   float2 coord;
};
vertex VertexOutput vertex_render(
   const device Facet *plane [[buffer(0)]],
   const device Vertex *point [[buffer(1)]],
   const device uint *order [[buffer(2)]],
   const device Object *object [[buffer(3)]],
   const device State *state [[buffer(4)]],
   uint id [[vertex_id]])
{
   VertexOutput out;
   uint ident = order[id];
   uint face = copoint(point[ident].plane,plane,state); // which plane of point is the face being rendered
   uint corner = coplane(plane[face].point,ident); // which color and coord in face is being rendered
   Triple triple = explode(point[ident].plane,plane,state); // planes defined by several points each
   out.position = convert(face,triple,plane,object,state);
   out.normal = normal3(triple);
   out.color = plane[face].color[corner];
   out.coord = plane[face].coord[corner];
   return out;
}
fragment half4 fragment_render(
   VertexOutput in [[stage_in]])
{
   return half4(in.color);
}
vertex VertexOutput vertex_simple(
   const device Facet *point [[buffer(0)]],
   uint ident [[vertex_id]])
{
   VertexOutput out = VertexOutput();
   out.position = float4(point[ident].plane,1.0);
   out.color = point[ident].color[0];
   return out;
}
kernel void kernel_pierce(
   const device Facet *plane [[buffer(0)]],
   const device Vertex *point [[buffer(1)]],
   const device uint *order [[buffer(2)]],
   const device Object *object [[buffer(3)]],
   const device State *state [[buffer(4)]],
   uint id [[thread_position_in_grid]],
   device Pierce *pierce [[buffer(5)]])
{
   uint ident = order[id];
   float3 near = float3(0.0); // float3(state->cursor,1.0);
   float3 far = float3(0.0); //
   Expand face = prepare(ident,plane,object,state);
   if (!opposite(face,near,far)) {
      pierce[ident].valid = false; return;}
   Qualify hole = intrasect(face,near,far);
   if (hole.quality == INFINITY) {
      pierce[ident].valid = false; return;}
   Triple edge;
   Expand apex;
   uint3 index;
   for (uint i = 0; i < 3; i++) {
      uint corner = plane[ident].point[i];
      Triple triple = explode(point[corner].plane,plane,state);
      apex.point[i] = convert(ident,triple,plane,object,state).xyz;
      for (uint j = 0; j < 3; j++) {
         uint line = point[corner].plane[j];
         bool found = false;
         for (uint k = 0; k < i; k++)
            if (index[k] == line) found = true;
         if (!found && line != ident) index[i] = line;}
      edge.plane[i] = expand(plane[index[i]],state);}
   for (uint i = 0; i < 3; i++)
      if (opposite(edge.plane[i],hole.point,apex.point[i])) {
         pierce[ident].valid = false; return;}
   pierce[ident].normal = normal(face);
   pierce[ident].point = hole.point;
   pierce[ident].valid = true;
}
struct Bytes {
   char bytes[12];
};
kernel void kernel_debug(
   const device Facet *plane [[buffer(0)]],
   const device Index *point [[buffer(1)]],
   uint ident [[thread_position_in_grid]],
   device Bytes *bytes [[buffer(2)]])
{
   bytes[ident].bytes[0] = (device char*)&plane[ident].plane - (device char*)&plane[ident];
   bytes[ident].bytes[1] = (device char*)&plane[ident].versor - (device char*)&plane[ident];
   bytes[ident].bytes[2] = (device char*)&plane[ident].point - (device char*)&plane[ident];
   bytes[ident].bytes[3] = (device char*)&plane[ident].coord[0] - (device char*)&plane[ident];
   bytes[ident].bytes[4] = (device char*)&plane[ident].color[0] - (device char*)&plane[ident];
   bytes[ident].bytes[5] = ((device char*)&plane[ident].poly - (device char*)&plane[ident]) - 128;
   bytes[ident].bytes[6] = ((device char*)&plane[ident].tag - (device char*)&plane[ident]) - 128;
   bytes[ident].bytes[7] = ((device char*)&plane[ident+1] - (device char*)&plane[ident]) - 128;
   bytes[ident].bytes[8] = (device char*)&point[ident+1] - (device char*)&point[ident];
   bytes[ident].bytes[9] = point[ident].point.x;
   bytes[ident].bytes[10] = point[ident].point.y;
   bytes[ident].bytes[11] = plane[ident].tag;
}
