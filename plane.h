/*
*    plane.h
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

#include <metal_matrix>

struct Facet {
	float3 plane; // distances above expanded basis plane
	uint versor; // which expanded plane in basis to use
	uint3 point; // match to point to choose coord and color
	float2 coord[3];
	float4 color[3];
	uint poly; // which polytope plane is in
	uint tag; // which of 7 sets plane is in
};
struct Vertex {
	uint3 plane; // planes intersect in point
};
struct Index {
	uint point;
	uint tag;
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
struct Expand {
	float3 point[3]; // plane expanded to points by basis
};
struct Triple { // expanded planes for intersect
	Expand plane[3];
};
struct Pierce { // intrasect plane with line through points
	bool valid; // valid if points opposite plane
	float3 point;
	float3 normal;
};
struct Object {
	metal::float4x4 object;
};
struct Cloud {
	float3 point;
};
struct State {
	float3 basis[3][3];
	metal::float4x4 subject;
	metal::float4x4 feature;
	float3 feather; // near for pierce
	float3 arrow; // far for pierce
	float3 focal; // focal for display
	float3 picture; // picture for display
	uint siz; // for bounce shader
	uint hand; // which plane to apply feature transformation to
	uint tag; // which planes of polytope to render this pass
};
