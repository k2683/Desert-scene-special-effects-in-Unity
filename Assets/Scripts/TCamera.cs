using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class TCamera : MonoBehaviour
{
    public Transform trans;
    public RenderTexture PathRT;
    // Start is called before the first frame update
    void Start()
    {
        
    }

    // Update is called once per frame
    void Update()
    {
        this.transform.position = trans.position + new Vector3(0 , 80 , 0);
        Shader.SetGlobalVector("_OrthographicCameraPos", this.transform.position);
        Shader.SetGlobalTexture("_PathRT", PathRT);
        Shader.SetGlobalFloat("_OrthographicCameraSize", GetComponent<Camera>().orthographicSize);
    }
}
