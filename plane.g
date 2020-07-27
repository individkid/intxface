#include "plane.h"

Expand expand(Plane plane, const device State *state)
{
   Expand result;
   for (uint i = 0; i < 3; i++) {
      result.point[i] = state->basis.plane[plane.versor].point[i];
      result.point[i][plane.versor] = plane.plane[i];}
   return result;
}
bool opposite(Expand plane, float3 feather, float3 arrow)
{
   return false;
}
bool inside(Expand plane[3], float3 corner[3], float3 point)
{
   for (uint i = 0; i < 3; i++)
      if (opposite(plane[i],corner[i],point-corner[i]))
         return false;
   return true;
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
   uint3 point,
   const device Plane *plane,
   const device State *state)
{
   uint face = 0;
   for (uint i = 0; i < 3; i++) {
      uint index = point[i];
      if (plane[index].tag == state->tag)
         face = index;}
   return face;
}
uint coplane(
   uint ident,
   uint face,
   const device Plane *plane)
{
   uint corner = 0;
   for (uint i = 0; i < 3; i++)
      if (plane[face].point[i] == ident)
         corner = i;
   return corner;
}
Triple explode(
   uint3 point,
   const device Plane *plane,
   const device State *state)
{
   Triple result;
   for (uint i = 0; i < 3; i++) {
      uint index = point[i];
      result.plane[i] = expand(plane[index],state);
      if (state->manipulate == index) for (uint j = 0; j < 3; j++) {
         float4 affine = float4(result.plane[i].point[j],1.0);
         result.plane[i].point[j] = (state->feature*affine).xyz;}}
   return result;
}
float4 convert(
   uint face,
   Triple triple,
   const device Plane *plane,
   const device State *state)
{
   float4 position = float4(intersect(triple),1.0);
   if (state->polytope == plane[face].polytope) position = state->object*position;
   position = state->subject*position;
   return position;
}
Expand prepare(
   uint ident,
   const device Plane *plane,
   const device State *state)
{
   Expand face = expand(plane[ident],state);
   if (state->manipulate == ident) face = transform(face,state->feature);
   if (state->polytope == plane[ident].polytope) transform(face,state->object);
   transform(face,state->subject);
   return face;
}
// MAIN
struct VertexOutput {
   float4 position [[position]];
   half3 normal;
   half4 color;
   half2 coord;
};
vertex VertexOutput vertex_render(
   const device Plane *plane [[buffer(0)]],
   const device Point *point [[buffer(1)]],
   uint ident [[vertex_id]],
   const device State *state)
{
   VertexOutput out;
   uint face = copoint(point[ident].plane,plane,state); // which plane of point is the face being rendered
   uint corner = coplane(ident,face,plane); // which color and coord in face is being rendered
   Triple triple = explode(point[ident].plane,plane,state); // planes defined by several points each
   out.position = convert(face,triple,plane,state);
   out.normal = half3(normal3(triple));
   out.color = half4(plane[face].color[corner]);
   out.coord = half2(plane[face].coord[corner]);
   return out;
}
fragment half4 fragment_render(
   VertexOutput in [[stage_in]])
{
   return in.color;
}
vertex void vertex_pierce(
   const device Plane *plane [[buffer(0)]],
   const device Point *point [[buffer(1)]],
   device Pierce *pierce [[buffer(2)]],
   uint ident [[vertex_id]],
   const device State *state)
{
   float3 feather = state->feather; // focal point
   float3 arrow = state->arrow; // line of sight
   Expand face = prepare(ident,plane,state);
   if (!opposite(face,feather,arrow)) {
      pierce[ident].valid = false; return;}
   float3 hole = intrasect(face,feather,arrow);
   Triple edge;
   Expand apex;
   uint3 index;
   for (uint i = 0; i < 3; i++) {
      uint corner = plane[ident].point[i];
      Triple triple = explode(point[corner].plane,plane,state);
      apex.point[i] = convert(ident,triple,plane,state).xyz;
      uint line;
      for (uint j = 0; j < 3; j++) {
         line = point[corner].plane[j];
         uint k;
         for (k = 0; k < i; k++)
            if (index[k] == line) break;
         if (k == i) break;}
      index[i] = line;
      edge.plane[i] = expand(plane[line],state);}
   for (uint i = 0; i < 3; i++)
      if (opposite(edge.plane[i],hole,apex.point[i])) {
         pierce[ident].valid = false; return;}
   pierce[ident].point = hole;
   pierce[ident].valid = true;
}
