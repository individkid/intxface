#include "plane.h"

Expand expand(Facet plane, const device State *state)
{
   Expand result;
   for (uint i = 0; i < 3; i++) {
      result.point[i] = state->basis.plane[plane.versor].point[i];
      result.point[i][plane.versor] = plane.plane[i];}
   return result;
}
bool opposite(Expand plane, float3 feather, float3 arrow)
{
   return false; // TODO
}
float3 intrasect(Expand plane, float3 feather, float3 arrow)
{
   return float3(0.0); // TODO
}
float3 intersect(Triple point)
{
   return float3(0.0); // TODO
}
float3 normal(Expand plane)
{
   return float3(0.0); // TODO
}
float3 average3(Expand plane)
{
   return float3(0.0); // TODO
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
   const device uint *refer [[buffer(2)]],
   const device Object *object [[buffer(3)]],
   const device State *state [[buffer(4)]],
   uint ident [[vertex_id]]) // TODO treat as refer index instead of point index
{
   VertexOutput out;
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
vertex void vertex_pierce(
   const device Facet *plane [[buffer(0)]],
   const device Vertex *point [[buffer(1)]],
   const device uint *refer [[buffer(2)]],
   const device Object *object [[buffer(3)]],
   const device State *state [[buffer(4)]],
   uint ident [[vertex_id]], // TODO treat as refer index instead of plane index
   device Pierce *pierce [[buffer(5)]])
{
   float3 feather = state->feather; // focal point
   float3 arrow = state->arrow; // mouse direction
   Expand face = prepare(ident,plane,object,state);
   if (!opposite(face,feather,arrow)) {
      pierce[ident].valid = false; return;}
   float3 hole = intrasect(face,feather,arrow);
   Triple edge;
   Expand apex;
   uint3 index;
   for (uint i = 0; i < 3; i++) {
      uint corner = plane[ident].point[i];
      Triple triple = explode(point[corner].plane,plane,state);
      apex.point[i] = convert(ident,triple,plane,object,state).xyz;
      uint line = 0;
      for (uint j = 0; j < 3; j++) {
         line = point[corner].plane[j];
         bool found = false;
         for (uint k = 0; k < i; k++)
            if (index[k] == line) found = true;
         if (!found && line != ident) index[i] = line;}
      edge.plane[i] = expand(plane[line],state);}
   for (uint i = 0; i < 3; i++)
      if (opposite(edge.plane[i],hole,apex.point[i])) {
         pierce[ident].valid = false; return;}
   pierce[ident].normal = normal(face);
   pierce[ident].point = hole;
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
