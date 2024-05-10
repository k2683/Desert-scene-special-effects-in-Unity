//Character Controller
using UnityEngine;

public class CharactorController : MonoBehaviour
{
    public float walkSpeed = 5.0f;
    public float runSpeed = 10.0f;
    public float turnSpeed = 10.0f; // 控制转向的速度
    public int camRotateSpeed = 20;
    public float jumpHeight = 10.0f; // 跳跃高度
    public float gravityValue = -9.81f; // 重力值
    public bool isJumping;
    private Vector3 playerVelocity; // 角色的速度
    private bool isGrounded; // 角色是否接触地面

    public Animator animator;
    public Camera myCamera;
    private CharacterController characterController;


    private bool isRunning = false; // 初始状态为走路

    void Start()
    {
        animator = GetComponent<Animator>();
        characterController = GetComponent<CharacterController>();
    }

    void Update()
    {
        isGrounded = characterController.isGrounded;
        if (isGrounded && playerVelocity.y < 0)
        {
            playerVelocity.y = -0.000f;
        }

        if (Input.GetKeyDown(KeyCode.LeftShift))
        {
            isRunning = !isRunning;
        }

        float horizontal = Input.GetAxis("Horizontal");
        float vertical = Input.GetAxis("Vertical");
        bool isMoving = horizontal != 0 || vertical != 0;
        animator.SetBool("IsWalking", isMoving && !isRunning);
        animator.SetBool("IsRunning", isMoving && isRunning);

        if (isMoving)
        {
            // 计算基于摄像机前方和右方的移动方向
            Vector3 forward = myCamera.transform.forward;
            Vector3 right = myCamera.transform.right;
            // 确保移动不受摄像机高度变化的影响
            forward.y = 0;
            right.y = 0;
            forward.Normalize();
            right.Normalize();

            // 将输入映射到摄像机方向
            Vector3 direction = forward * vertical + right * horizontal;

            // 根据当前状态选择速度
            float speed = isRunning ? runSpeed : walkSpeed;

            // 移动角色控制器
            characterController.Move(direction.normalized * speed * Time.deltaTime);

            // 转向处理
            if (direction != Vector3.zero)
            {
                Quaternion toRotation = Quaternion.LookRotation(direction, Vector3.up);
                transform.rotation = Quaternion.Slerp(transform.rotation, toRotation, turnSpeed * Time.deltaTime);
            }
        }
        //Debug.Log(isGrounded);

        //if (Input.GetButtonDown("Jump") && isGrounded)
        //if(Input.GetKeyDown("space") && isGrounded)
        //if (Input.GetKeyDown("space") )
        if (Input.GetKey("space") && isGrounded)
        {
            playerVelocity.y += Mathf.Sqrt(jumpHeight * -3.0f * gravityValue);
            //animator.SetBool("IsJumping", true);
            animator.SetTrigger("Jump");
            isJumping = true;
        }
        else if(isGrounded)
        {
            //animator.SetBool("IsJumping", false);

        }
        // 应用重力
        playerVelocity.y += gravityValue * Time.deltaTime*70;
        characterController.Move(playerVelocity * Time.deltaTime);
        //Debug.Log(playerVelocity.y);

    }

}
