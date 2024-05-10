Shader "MixSDF"
{
    Properties
    {
        _SDFTex0 ("SDFTex0" , 2D) = "black"{}
        _SDFTex1 ("SDFTex1" , 2D) = "black"{}
        _Blend ("Blend" , Range(0 , 1)) = 0
        _Step("Step" , Range(0.00001 , 1)) = 0
    }

    SubShader
    {
        Tags
        {
            "RanderPipline" = "UniversalPipeline"
            "RanderType" = "Opaque"
        }

        HLSLINCLUDE
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
            CBUFFER_START(UnityPerMaterial)
                float _Blend;
                float _Step;
            CBUFFER_END

            TEXTURE2D (_SDFTex0);
            TEXTURE2D (_SDFTex1);
            SAMPLER(sampler_SDFTex0);
            SAMPLER(sampler_SDFTex1);           
        ENDHLSL

        Pass
        {
            Name "ForwardUnlit"
            Tags {"LightMode" = "UniversalForward"}

            HLSLPROGRAM

                #pragma vertex vert
                #pragma fragment frag

                struct Attributes
            {
                float4 positionOS : POSITION;
                float2 uv         : TEXCOORD0;
            };

            struct Varyings
            {
                float4 positionCS : SV_POSITION;
                float2 uv         : TEXCOORD0;
            };
               
                Varyings vert(Attributes input)
                {
                    
                    const VertexPositionInputs vertexInput = GetVertexPositionInputs(input.positionOS.xyz);
                    Varyings output;
                    output.uv = input.uv;
                    output.positionCS = vertexInput.positionCS;
                    return output;
                }


                half4 frag(Varyings input) : SV_Target
                {
                    half4 sdf0 = SAMPLE_TEXTURE2D(_SDFTex0, sampler_SDFTex0, input.uv);
                    half4 sdf1 = SAMPLE_TEXTURE2D(_SDFTex1, sampler_SDFTex1, input.uv);
                    sdf0.r = pow(sdf0.r , 0.1);
                    sdf1.r = pow(sdf1.r , 0.5);
                    half sdf = lerp(sdf0.r , sdf1.r , _Blend);
                    half final = step(_Step , sdf);
                    return half4(final.xxx , 1);
                }
            
            ENDHLSL
        }
    }
}