Shader "Scenes/Grass"
{
    Properties
    {
        [Toggle(UNLIT)]_UnlitMode("无光模式", Float) = 0
        [Toggle]_UseTexColor("使用贴图颜色", Float) = 0
        [Toggle(SHADOW)]_SHADOW("实时阴影", Float) = 0
        _MainTex ("Texture", 2D) = "white" {}
        _Brightness("明度",range(1,3))=1
        _AlphaCutoff("Alpha Cutoff", Range( 0 , 1)) = 0.35
        _AmbientIntensity("环境光",float)=1
        [Header(Color Settings)][Space(5)][KeywordEnum(UV,VertexPosY)] _ColorGradient("颜色渐变", Float) = 0
        _TopTint ("顶部颜色",color) = (1,1,1,1)
        _BottomTint ("底部颜色",color) = (1,1,1,1)
        _Fade("渐变控制", float) = 1
        [Toggle(BLEND)]_BlendFloor("地形颜色融合", Float) = 0
        _BlendFade("混合控制", float) = 1

        [Header(Wind)]
        [KeywordEnum(UV,VertexPosY)] _WindGradient("风力控制", Float) = 0
        _WindSpeed("风速",range(1,50)) = 20
        _WindStrength("风强",range(0,1))=0.1
        _Stretch("拉伸",Vector) = (0,1,0,0)
    }
    SubShader
    {
        Tags { "RenderType" = "Opaque" "RenderPipeline" = "UniversalPipeline" "IgnoreProjector" = "True" }
        //LOD 100

        Pass
        {
            Tags { "LightMode" = "UniversalForward" }
            Cull Off
            HLSLPROGRAM
            // Lighting and shadow keywords
            #pragma multi_compile _ _MAIN_LIGHT_SHADOWS _MAIN_LIGHT_SHADOWS_CASCADE _MAIN_LIGHT_SHADOWS_SCREEN
            #pragma multi_compile _ _ADDITIONAL_LIGHTS
            #pragma multi_compile _ _ADDITIONAL_LIGHT_SHADOWS
            #pragma multi_compile _ _SHADOWS_SOFT

            #pragma shader_feature_local_fragment BLEND
            #pragma shader_feature_local_fragment UNLIT
            #pragma shader_feature_local_fragment SHADOW
            #pragma shader_feature_local_fragment _ _COLORGRADIENT_UV _COLORGRADIENT_VERTEXPOSY
            #pragma shader_feature _ _WINDGRADIENT_UV _WINDGRADIENT_VERTEXPOSY
            #pragma multi_compile_fog
            // GPU Instancing
            #pragma multi_compile_instancing
            #pragma instancing_options renderinglayer
            #pragma multi_compile _ DOTS_INSTANCING_ON

            #pragma vertex vert
            #pragma fragment frag

            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"

            struct appdata
            {
                UNITY_VERTEX_INPUT_INSTANCE_ID
                float4 vertex : POSITION;
                float2 uv : TEXCOORD0;
                float4 color : COLOR;
            };

            struct v2f
            {
                UNITY_VERTEX_INPUT_INSTANCE_ID
                float2 uv : TEXCOORD0;
                float4 vertex : SV_POSITION;
                float4 positionWS : TEXCOORD1;
                float4 positionOS : TEXCOORD2;
            };

            sampler2D _MainTex;
            half4 _MainTex_ST;
            float _AlphaCutoff;
            float _UseTexColor;
            float3 _CharacterPos;
            float _InteractorRadius;
            float _InteractorStrength;
            float4 _Stretch;
            float _OrthographicCamSize;
            float4 _OrthographicCamPos;
            float _AmbientIntensity;
            sampler2D _TerrainDiffuse;

            UNITY_INSTANCING_BUFFER_START(Props)
            UNITY_DEFINE_INSTANCED_PROP(half4, _TopTint)
            UNITY_DEFINE_INSTANCED_PROP(half4, _BottomTint)
            UNITY_DEFINE_INSTANCED_PROP(float, _Fade)
            UNITY_DEFINE_INSTANCED_PROP(float, _BlendFade)
            UNITY_DEFINE_INSTANCED_PROP(float, _WindSpeed)
            UNITY_DEFINE_INSTANCED_PROP(float, _WindStrength)
            UNITY_DEFINE_INSTANCED_PROP(half, _Brightness)
            UNITY_INSTANCING_BUFFER_END(Props)

            v2f vert(appdata v)
            {
                v2f o;
                UNITY_SETUP_INSTANCE_ID(v);
                UNITY_TRANSFER_INSTANCE_ID(v, o);

                float3 positionWS = mul(UNITY_MATRIX_M, float4(v.vertex.xyz, 1.0));

                float3 Wind = float3(sin(_Time.x * UNITY_ACCESS_INSTANCED_PROP(Props, _WindSpeed) + positionWS.x) + sin(
                                         _Time.x * UNITY_ACCESS_INSTANCED_PROP(Props, _WindSpeed) + positionWS.z * 2) +
                                     sin(
                                         _Time.x * UNITY_ACCESS_INSTANCED_PROP(Props, _WindSpeed) * 0.1 + positionWS.x),
                                     0,
                                     cos(_Time.x * UNITY_ACCESS_INSTANCED_PROP(Props, _WindSpeed) + positionWS.x * 2) +
                                     cos(
                                         _Time.x * UNITY_ACCESS_INSTANCED_PROP(Props, _WindSpeed) + positionWS.z));
                Wind *= UNITY_ACCESS_INSTANCED_PROP(Props, _WindStrength);

                float3 playerToVertex = positionWS - _CharacterPos;
                float3 directionFromPlayer = normalize(playerToVertex);
                float distanceFromSphere = abs(length(playerToVertex)) + _InteractorRadius;
                float3 baseXZOffset = float3(directionFromPlayer.x, 0, directionFromPlayer.z) * distanceFromSphere;
                float3 sphereDisp = (baseXZOffset * _InteractorStrength) - float3(0, distanceFromSphere*  1, 0);
                float3 dis = distance(_CharacterPos, positionWS);
                float3 radius = 1 - saturate(dis / _InteractorRadius);
                sphereDisp *= radius ;

                float3 Stretch = normalize( float3(_Stretch.x , 1.0 , _Stretch.z) );
				Stretch = (float3(Stretch.x , 0.0 , Stretch.z));

                #if defined(_WINDGRADIENT_UV)
			    float windFade = v.uv.y;
			    #else
			    float windFade = saturate(v.vertex.y);
			    #endif

                positionWS.xyz += (sphereDisp + Wind + Stretch) * windFade;

                o.positionWS.xyz = positionWS;
                o.uv = TRANSFORM_TEX(v.uv, _MainTex);
                o.vertex = TransformWorldToHClip(positionWS);
                o.positionWS.w = ComputeFogFactor(o.vertex.z);
                o.positionOS = v.vertex;

                return o;
            }
            inline float3 ACES_Tonemapping(float3 x)
			{
				float a = 2.51f;
				float b = 0.03f;
				float c = 2.43f;
				float d = 0.59f;
				float e = 0.14f;
				float3 encode_color = saturate((x*(a*x + b)) / (x*(c*x + d) + e));
				return encode_color;
			};
            half4 frag(v2f i) : SV_Target
            {
                UNITY_SETUP_INSTANCE_ID(i);
                half4 col = tex2D(_MainTex, i.uv);
                clip(col.a - _AlphaCutoff);
                col.rgb = lerp(1.0,col.rgb,_UseTexColor) * UNITY_ACCESS_INSTANCED_PROP(Props, _Brightness);

                // fade over the length of the grass
                #if defined(_COLORGRADIENT_UV)
			    half colorFade = i.uv.y;
			    #elif defined(_COLORGRADIENT_VERTEXPOSY)
			    half colorFade = i.positionOS.y;
			    #else
			    half colorFade = i.positionOS.y;
			    #endif

                half verticalFade = saturate(colorFade * UNITY_ACCESS_INSTANCED_PROP(Props, _Fade));

                half4 baseColor = lerp(UNITY_ACCESS_INSTANCED_PROP(Props, _BottomTint) , UNITY_ACCESS_INSTANCED_PROP(Props, _TopTint), verticalFade) * col;

                half3 final = half3(0,0,0);

                float3 WorldPos = i.positionWS.xyz;
                #if defined(_MAIN_LIGHT_SHADOWS_SCREEN)
	                float4 positionCS = TransformWorldToHClip(WorldPos);
                    float4 ShadowCoord = ComputeScreenPos(positionCS);
	            #else
                    float4 ShadowCoord = TransformWorldToShadowCoord(WorldPos);
	            #endif
	            half4 ShadowMask = float4(1.0,1.0,1.0,1.0);

                #if UNLIT
                    final = baseColor;
                    #if SHADOW 
                    Light mainlight = GetMainLight(ShadowCoord,WorldPos,ShadowMask);
                    half MainShadow = mainlight.shadowAttenuation;
                    #else
                    half MainShadow = 1.0f;
                    #endif
                #else
                    Light mainlight = GetMainLight(ShadowCoord,WorldPos,ShadowMask);
                    half MainShadow = mainlight.shadowAttenuation;
                    half3 LightDir = mainlight.direction;
                    half3 LightColor = mainlight.color * mainlight.distanceAttenuation;
                    half NdotL = clamp(dot(float3(0,1,0), LightDir),0.01,1);
                    final = baseColor * NdotL * LightColor;
                #endif

                //final = ACES_Tonemapping(final);

                #if BLEND
                    float2 uv = i.positionWS.xz - _OrthographicCamPos.xz;
                    uv = uv / (_OrthographicCamSize * 2);
                    uv += 0.5;
                    half4 terrainForBlending = tex2D(_TerrainDiffuse, uv);
                    half blendFade = saturate(colorFade * UNITY_ACCESS_INSTANCED_PROP(Props, _BlendFade));
                    final = lerp(terrainForBlending, final, blendFade);
                #endif

                final *= MainShadow;
                half3 ambient = half3(unity_SHAr.w, unity_SHAg.w, unity_SHAb.w);
                final += ambient * baseColor * (1 - MainShadow) * _AmbientIntensity;

                //附加光源
                half3 DirectLighting_AddLight = half3(0,0,0);
                {
                    #ifdef _ADDITIONAL_LIGHTS
                    uint pixelLightCount = GetAdditionalLightsCount();
                    for(uint lightIndex = 0; lightIndex < pixelLightCount ; ++lightIndex)
                    {
                        Light light = GetAdditionalLight(lightIndex,WorldPos,ShadowMask);
                        half3 LightColor = light.color * light.distanceAttenuation;
                        DirectLighting_AddLight += LightColor * light.shadowAttenuation;
                    }
                    #endif
                }
                DirectLighting_AddLight *= verticalFade;
                final += DirectLighting_AddLight;

                // Mix the pixel color with fogColor. 
                final = MixFog(final, i.positionWS.w);      
            
                return half4(final,1);

            }
            ENDHLSL
        }
        Pass
        {
            Name "DepthOnly"
            Tags{"LightMode" = "DepthOnly"}

            ZWrite On
            ColorMask 0
            Cull off

            HLSLPROGRAM

            #pragma vertex vert
            #pragma fragment frag

            // -------------------------------------
            // Material Keywords
            #pragma shader_feature_local_fragment _ALPHATEST_ON
            #pragma shader_feature_local_fragment _SMOOTHNESS_TEXTURE_ALBEDO_CHANNEL_A

            //--------------------------------------
            // GPU Instancing
            #pragma multi_compile_instancing
            #pragma multi_compile _ DOTS_INSTANCING_ON

            #include "Packages/com.unity.render-pipelines.universal/Shaders/LitInput.hlsl"
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"

            struct appdata
            {
                UNITY_VERTEX_INPUT_INSTANCE_ID
                float4 vertex : POSITION;
                float2 uv : TEXCOORD0;
                float4 color : COLOR;
            };

            struct v2f
            {
                UNITY_VERTEX_INPUT_INSTANCE_ID
                float2 uv : TEXCOORD0;
                float4 vertex : SV_POSITION;
                float4 positionWS : TEXCOORD1;
                float4 positionOS : TEXCOORD2;
            };

            sampler2D _MainTex;
            half4 _MainTex_ST;
            float _AlphaCutoff;
            float _UseTexColor;
            float3 _CharacterPos;
            float _InteractorRadius;
            float _InteractorStrength;
            float4 _Stretch;
            float _OrthographicCamSize;
            float4 _OrthographicCamPos;
            float _AmbientIntensity;
            sampler2D _TerrainDiffuse;

            UNITY_INSTANCING_BUFFER_START(Props)
            UNITY_DEFINE_INSTANCED_PROP(half4, _TopTint)
            UNITY_DEFINE_INSTANCED_PROP(half4, _BottomTint)
            UNITY_DEFINE_INSTANCED_PROP(float, _Fade)
            UNITY_DEFINE_INSTANCED_PROP(float, _BlendFade)
            UNITY_DEFINE_INSTANCED_PROP(float, _WindSpeed)
            UNITY_DEFINE_INSTANCED_PROP(float, _WindStrength)
            UNITY_DEFINE_INSTANCED_PROP(half, _Brightness)
            UNITY_INSTANCING_BUFFER_END(Props)

            v2f vert(appdata v)
            {
                v2f o;
                UNITY_SETUP_INSTANCE_ID(v);
                UNITY_TRANSFER_INSTANCE_ID(v, o);

                float3 positionWS = mul(UNITY_MATRIX_M, float4(v.vertex.xyz, 1.0));

                float3 Wind = float3(sin(_Time.x * UNITY_ACCESS_INSTANCED_PROP(Props, _WindSpeed) + positionWS.x) + sin(
                                         _Time.x * UNITY_ACCESS_INSTANCED_PROP(Props, _WindSpeed) + positionWS.z * 2) +
                                     sin(
                                         _Time.x * UNITY_ACCESS_INSTANCED_PROP(Props, _WindSpeed) * 0.1 + positionWS.x),
                                     0,
                                     cos(_Time.x * UNITY_ACCESS_INSTANCED_PROP(Props, _WindSpeed) + positionWS.x * 2) +
                                     cos(
                                         _Time.x * UNITY_ACCESS_INSTANCED_PROP(Props, _WindSpeed) + positionWS.z));
                Wind *= UNITY_ACCESS_INSTANCED_PROP(Props, _WindStrength);

                float3 playerToVertex = positionWS - _CharacterPos;
                float3 directionFromPlayer = normalize(playerToVertex);
                float distanceFromSphere = abs(length(playerToVertex)) + _InteractorRadius;
                float3 baseXZOffset = float3(directionFromPlayer.x, 0, directionFromPlayer.z) * distanceFromSphere;
                float3 sphereDisp = (baseXZOffset * _InteractorStrength) - float3(0, distanceFromSphere*  1, 0);
                float3 dis = distance(_CharacterPos, positionWS);
                float3 radius = 1 - saturate(dis / _InteractorRadius);
                sphereDisp *= radius ;

                float3 Stretch = normalize( float3(_Stretch.x , 1.0 , _Stretch.z) );
				Stretch = (float3(Stretch.x , 0.0 , Stretch.z));

                #if defined(_WINDGRADIENT_UV)
			    float windFade = v.uv.y;
			    #else
			    float windFade = saturate(v.vertex.y);
			    #endif

                positionWS.xyz += (sphereDisp + Wind + Stretch) * windFade;

                o.positionWS.xyz = positionWS;
                o.uv = TRANSFORM_TEX(v.uv, _MainTex);
                o.vertex = TransformWorldToHClip(positionWS);
                o.positionWS.w = ComputeFogFactor(o.vertex.z);
                o.positionOS = v.vertex;

                return o;
            }

            half4 frag(v2f i) : SV_Target
            {
                UNITY_SETUP_INSTANCE_ID(i);
                half4 col = tex2D(_MainTex, i.uv);
                clip(col.a - _AlphaCutoff);
            
                return 0;

            }
            ENDHLSL
        }

        // This pass is used when drawing to a _CameraNormalsTexture texture
        Pass
        {
            Name "DepthNormals"
            Tags{"LightMode" = "DepthNormals"}

            ZWrite On
            Cull off

            HLSLPROGRAM

            #pragma vertex vert
            #pragma fragment frag

            // -------------------------------------
            // Material Keywords
            #pragma shader_feature_local _NORMALMAP
            #pragma shader_feature_local _PARALLAXMAP
            #pragma shader_feature_local _ _DETAIL_MULX2 _DETAIL_SCALED
            #pragma shader_feature_local_fragment _ALPHATEST_ON
            #pragma shader_feature_local_fragment _SMOOTHNESS_TEXTURE_ALBEDO_CHANNEL_A

            //--------------------------------------
            // GPU Instancing
            #pragma multi_compile_instancing
            #pragma multi_compile _ DOTS_INSTANCING_ON

            #include "Packages/com.unity.render-pipelines.universal/Shaders/LitInput.hlsl"
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"

            struct appdata
            {
                UNITY_VERTEX_INPUT_INSTANCE_ID
                float4 vertex : POSITION;
                float2 uv : TEXCOORD0;
                float4 color : COLOR;
                float3 normalOS : NORMAL;
            };

            struct v2f
            {
                UNITY_VERTEX_INPUT_INSTANCE_ID
                float2 uv : TEXCOORD0;
                float4 vertex : SV_POSITION;
                float4 positionWS : TEXCOORD1;
                float4 positionOS : TEXCOORD2;
                float3 normalWS : TEXCOORD3;
            };

            sampler2D _MainTex;
            half4 _MainTex_ST;
            float _AlphaCutoff;
            float _UseTexColor;
            float3 _CharacterPos;
            float _InteractorRadius;
            float _InteractorStrength;
            float4 _Stretch;
            float _OrthographicCamSize;
            float4 _OrthographicCamPos;
            float _AmbientIntensity;
            sampler2D _TerrainDiffuse;

            UNITY_INSTANCING_BUFFER_START(Props)
            UNITY_DEFINE_INSTANCED_PROP(half4, _TopTint)
            UNITY_DEFINE_INSTANCED_PROP(half4, _BottomTint)
            UNITY_DEFINE_INSTANCED_PROP(float, _Fade)
            UNITY_DEFINE_INSTANCED_PROP(float, _BlendFade)
            UNITY_DEFINE_INSTANCED_PROP(float, _WindSpeed)
            UNITY_DEFINE_INSTANCED_PROP(float, _WindStrength)
            UNITY_DEFINE_INSTANCED_PROP(half, _Brightness)
            UNITY_INSTANCING_BUFFER_END(Props)

            v2f vert(appdata v)
            {
                v2f o;
                UNITY_SETUP_INSTANCE_ID(v);
                UNITY_TRANSFER_INSTANCE_ID(v, o);

                float3 positionWS = mul(UNITY_MATRIX_M, float4(v.vertex.xyz, 1.0));

                float3 Wind = float3(sin(_Time.x * UNITY_ACCESS_INSTANCED_PROP(Props, _WindSpeed) + positionWS.x) + sin(
                                         _Time.x * UNITY_ACCESS_INSTANCED_PROP(Props, _WindSpeed) + positionWS.z * 2) +
                                     sin(
                                         _Time.x * UNITY_ACCESS_INSTANCED_PROP(Props, _WindSpeed) * 0.1 + positionWS.x),
                                     0,
                                     cos(_Time.x * UNITY_ACCESS_INSTANCED_PROP(Props, _WindSpeed) + positionWS.x * 2) +
                                     cos(
                                         _Time.x * UNITY_ACCESS_INSTANCED_PROP(Props, _WindSpeed) + positionWS.z));
                Wind *= UNITY_ACCESS_INSTANCED_PROP(Props, _WindStrength);

                float3 playerToVertex = positionWS - _CharacterPos;
                float3 directionFromPlayer = normalize(playerToVertex);
                float distanceFromSphere = abs(length(playerToVertex)) + _InteractorRadius;
                float3 baseXZOffset = float3(directionFromPlayer.x, 0, directionFromPlayer.z) * distanceFromSphere;
                float3 sphereDisp = (baseXZOffset * _InteractorStrength) - float3(0, distanceFromSphere*  1, 0);
                float3 dis = distance(_CharacterPos, positionWS);
                float3 radius = 1 - saturate(dis / _InteractorRadius);
                sphereDisp *= radius ;

                float3 Stretch = normalize( float3(_Stretch.x , 1.0 , _Stretch.z) );
				Stretch = (float3(Stretch.x , 0.0 , Stretch.z));

                #if defined(_WINDGRADIENT_UV)
			    float windFade = v.uv.y;
			    #else
			    float windFade = saturate(v.vertex.y);
			    #endif

                positionWS.xyz += (sphereDisp + Wind + Stretch) * windFade;

                o.positionWS.xyz = positionWS;
                o.uv = TRANSFORM_TEX(v.uv, _MainTex);
                o.vertex = TransformWorldToHClip(positionWS);
                o.positionWS.w = ComputeFogFactor(o.vertex.z);
                o.positionOS = v.vertex;
                o.normalWS = normalize(TransformObjectToWorldNormal(v.normalOS));
                return o;
            }

            half4 frag(v2f i) : SV_Target
            {
                UNITY_SETUP_INSTANCE_ID(i);
                half4 col = tex2D(_MainTex, i.uv);
                clip(col.a - _AlphaCutoff);
                #if defined(_GBUFFER_NORMALS_OCT)
                float3 normalWS = normalize(i.normalWS);
                float2 octNormalWS = PackNormalOctQuadEncode(normalWS);           // values between [-1, +1], must use fp32 on some platforms.
                float2 remappedOctNormalWS = saturate(octNormalWS * 0.5 + 0.5);   // values between [ 0,  1]
                half3 packedNormalWS = PackFloat2To888(remappedOctNormalWS);      // values between [ 0,  1]
                return half4(packedNormalWS, 0.0);
                #else
                float3 normalWS = NormalizeNormalPerPixel(i.normalWS);
                return half4(normalWS, 0.0);
                #endif
            }
            ENDHLSL
        }

    }
}