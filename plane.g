#include "plane.h"
struct VertexOutput {
   float4 position [[position]];
   half3 normal;
   half4 color;
   half2 coord;
};
float3 expand(float3 plane, uint versor, float3 basis[3][3], int corner)
{
   return float3(0.0); // TODO
}
float3 intersect(float3 point[3][3])
{
   return float3(0.0); // TODO
}
float3 normal(float3 point[3])
{
   return float3(0.0); // TODO
}
float3 average3(float3 point[3])
{
   return float3(0.0); // TODO
}
float4 transform(float4 point, float4 matrix[4])
{
   return float4(0.0); // TODO
}
// MAIN
vertex VertexOutput vertex_main (
   const device Plane *plane [[buffer(0)]],
   const device Point *point [[buffer(1)]],
   uint ident [[vertex_id]],
   const device State *state)
{
   VertexOutput v_out;
   float3 basis[3][3];
   for (uint i = 0; i < 3; i++)
      for (uint j = 0; j < 3; j++)
         basis[i][j] = state->basis[i][j];
   float4 subject[4];
   for (uint i = 0; i < 4; i++)
      subject[i] = state->subject[i];
   float4 object[4];
   for (uint i = 0; i < 4; i++)
      object[i] = state->object[i];
   uint polytope = state->polytope;
   float4 feature[4];
   for (uint i = 0; i < 4; i++)
      feature[i] = state->feature[i];
   uint manipulate = state->manipulate;
   int face = 0; // which plane of point is the face being rendered
   for (uint i = 0; i < 3; i++) {
      uint index = point[ident].plane[i];
      if (plane[index].tag == state->tag)
         face = index;}
   int corner = 0; // which color and coord in face is being rendered
   for (uint i = 0; i < 3; i++)
      if (plane[face].point[i] == ident)
         corner = i;
   float3 attach[3][3]; // plane defined by several points on it
   for (uint i = 0; i < 3; i++) {
      uint index = point[ident].plane[i];
      float3 vector = plane[index].plane;
      uint versor = plane[index].versor;
      for (uint j = 0; j < 3; j++) {
         attach[i][j] = expand(vector,versor,basis,j);
         if (manipulate == index) {
            float4 affine = float4(attach[i][j],1.0);
            attach[i][j] = transform(affine,feature).xyz;}}}
   float4 position = float4(intersect(attach),1.0);
   if (polytope == plane[face].polytope)
      position = transform(position,object);
   v_out.position = transform(position,subject);
   float3 normed[3];
   for (uint i = 0; i < 3; i++)
      normed[i] = normal(attach[i]);
   v_out.normal = half3(average3(normed));
   v_out.color = half4(plane[face].color[corner]);
   v_out.coord = half2(plane[face].coord[corner]);
   return v_out;
}
fragment half4 fragment_main (
   VertexOutput in [[stage_in]])
{
   return in.color;
}

