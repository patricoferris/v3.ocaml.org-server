---
title: 'Secure From the Ground Up: Introducing the FIDES Project Combining RISC-V
  and MirageOS'
description: "We entrust some of our most sensitive information to an invisible stream
  of information flowing back and forth across the globe\u2026"
url: https://tarides.com/blog/2024-06-05-secure-from-the-ground-up-introducing-the-fides-project-combining-risc-v-and-mirageos
date: 2024-06-05T00:00:00-00:00
preview_image: https://tarides.com/static/f4dbf1b8f4f82190e376f8d109f43012/0132d/processor.jpg
featured:
authors:
- Tarides
source:
---

<p>We entrust some of our most sensitive information to an invisible stream of information flowing back and forth across the globe. Cybersecurity concerns everyone, and Tarides is developing secure, lasting, and high-performing solutions for a diverse set of users. This post introduces you to cutting-edge technology, employing some of the latest research in software development and cybersecurity to give you a sense of what the future of hardware and software security looks like.</p>
<p>Tarides is collaborating with the <a href="https://www.iitm.ac.in/">Indian Institute of Technology (IIT), Madras</a> on the FIDES project. The project employs a hardware-enabled intra-process compartmentalisation technique on the <a href="https://shakti.org.in/">Shakti</a> <a href="https://riscv.org/">RISC-V</a> processor, capable of running bare-metal <a href="https://mirage.io/">MirageOS</a> unikernels written in <a href="https://ocaml.org/">OCaml</a> safely alongside C code. This combined hardware and software solution leverages hardware-enabled compartments and OCaml's safety guarantees to ensure excellent security for real-world applications that mix safe and unsafe code.</p>
<h2 style="position:relative;"><a href="https://tarides.com/feed.xml#why-choose-risc-v-and-shakti" aria-label="why choose risc v and shakti permalink" class="anchor before"><svg aria-hidden="true" focusable="false" height="16" version="1.1" viewbox="0 0 16 16" width="16"><path fill-rule="evenodd" d="M4 9h1v1H4c-1.5 0-3-1.69-3-3.5S2.55 3 4 3h4c1.45 0 3 1.69 3 3.5 0 1.41-.91 2.72-2 3.25V8.59c.58-.45 1-1.27 1-2.09C10 5.22 8.98 4 8 4H4c-.98 0-2 1.22-2 2.5S3 9 4 9zm9-3h-1v1h1c1 0 2 1.22 2 2.5S13.98 12 13 12H9c-.98 0-2-1.22-2-2.5 0-.83.42-1.64 1-2.09V6.25c-1.09.53-2 1.84-2 3.25C6 11.31 7.55 13 9 13h4c1.45 0 3-1.69 3-3.5S14.5 6 13 6z"></path></svg></a>Why Choose RISC-V and Shakti?</h2>
<p>You might have heard of <a href="https://arxiv.org/pdf/2309.11332.pdf">CHERI</a>, a UK-based project that adds security features on top of <a href="https://en.wikipedia.org/wiki/ARM_architecture_family">ARM</a>. As of 2022 ARM is the most widely used family of ISAs, and ARM processors power anything from smartphones to supercomputers.</p>
<p>The goal of the FIDES project is similar to CHERI. It extends the open-source Shakti RISC-V processors with additional security features. Unlike CHERI, FIDES is specialised to run MirageOS applications on top. <a href="https://mirage.io/">MirageOS</a> is a library operating system that uses properties of <a href="https://ocaml.org/">OCaml</a> (a <a href="https://tarides.com/blog/2023-08-17-your-programming-language-and-its-impact-on-the-cybersecurity-of-your-application/">highly-secure programming language</a>) to construct Unikernels to make secure, high-performance, network applications compatible with a variety of cloud computing and mobile platforms. The key benefit of the FIDES approach is that we can take advantage of the language-level security offered by OCaml to alleviate some of the overheads of enforcing security purely through hardware. With FIDES, the user gets a secure system from the hardware all the way up to the application layer.</p>
<p>But what is RISC-V? In 2015, the University of California, Berkeley, wanted to encourage <a href="https://riscv.org/about/history/">&quot;an open, collaborative community of software and hardware innovators based on the RISC-V ISA&quot;</a> by founding the <a href="https://riscv.org/">RISC-V International Foundation</a>. &lsquo;RISC&rsquo; stands for &lsquo;Reduced Instruction Set Computer&rsquo; and RISC-V is an open standard Instruction Set Architecture (ISA), offered free of charge alongside its extensions for anyone looking to build solutions and services. The initiative has sparked countless projects since its inception.</p>
<p>Using the RISC-V design and ISA is already a good start for security. It has strong academic roots, originally developed from a series of computer design projects by experts at Berkeley. This solid foundation has only been strengthened by being open-source, where the entire RISC-V architecture can be scrutinised in the public domain. Unlike in closed-source projects where they can't see the code, users can be sure there are no back doors or hidden channels. Having a free, open-source alternative also increases competition in the sector; developers can use publicly available extensions and reference designs to create more secure options for the closed-source solutions available.</p>
<p>The <a href="https://shakti.org.in/">Shakti</a> initiative is an open-source project spearheaded by the Reconfigurable Intelligent Systems Engineering (RISE) team at IIT Madras. The project encompasses a family of processors that use the RISC-V ISA to create scalable, cost-efficient, and open-source hardware solutions.</p>
<p>The FIDES project combines the Shakti processor (using the RISC-V ISA), and runs secure MirageOS OCaml unikernels on top.</p>
<h2 style="position:relative;"><a href="https://tarides.com/feed.xml#the-fides-approach" aria-label="the fides approach permalink" class="anchor before"><svg aria-hidden="true" focusable="false" height="16" version="1.1" viewbox="0 0 16 16" width="16"><path fill-rule="evenodd" d="M4 9h1v1H4c-1.5 0-3-1.69-3-3.5S2.55 3 4 3h4c1.45 0 3 1.69 3 3.5 0 1.41-.91 2.72-2 3.25V8.59c.58-.45 1-1.27 1-2.09C10 5.22 8.98 4 8 4H4c-.98 0-2 1.22-2 2.5S3 9 4 9zm9-3h-1v1h1c1 0 2 1.22 2 2.5S13.98 12 13 12H9c-.98 0-2-1.22-2-2.5 0-.83.42-1.64 1-2.09V6.25c-1.09.53-2 1.84-2 3.25C6 11.31 7.55 13 9 13h4c1.45 0 3-1.69 3-3.5S14.5 6 13 6z"></path></svg></a>The FIDES Approach</h2>
<p>FIDES provides two extensions on top of the base Shakti RISC-V code. The first is a fine-grained compartmentalisation scheme that splits a process into multiple logical code partitions with an explicit access policy. This restricts what code can be called from functions in a given compartment. For example, the code of the display driver in a <a href="https://en.wikipedia.org/wiki/Payment_terminal">point-of-sale device</a> has no reason to call into crypto code. Therefore, the two can be statically partitioned into separate compartments. This prevents vulnerabilities in the display driver from affecting the crypto modules. In addition, using a language like <a href="https://tarides.com/blog/2023-07-05-zero-day-attacks-what-are-they-and-can-a-language-like-ocaml-protect-you/">OCaml guarantees memory- and temporal- safety</a> through its compiler and garbage collector. Thus, the programming language mitigates common memory error vulnerabilities while the compartmentalisation scheme mitigates privilege escalation vulnerabilities.</p>
<p>The second extension that FIDES provides is a fat-pointer scheme for C code. A significant amount of code is written in C, and we want to be able to make use of those programs without compromising on security. When C programs are combined with OCaml programs in the same application (such as a MirageOS unikernel), the vulnerabilities in the C code can compromise the security guarantees of the OCaml code. Every MirageOS unikernel has a significant chunk of C code present in the OCaml runtime that connects external libraries implementing core components such as fast crypto modules.</p>
<p>The FIDES fat pointer augments a pointer to a memory region with additional information that records the address range of a memory region (which is used for spatial memory safety) and a temporal cookie (for temporal memory safety). The spatial and temporal safety is checked on every access. The fat-pointer scheme ensures that the C code also has spatial and temporal memory safety. In this way, fat-pointers for C ensure that when linked with OCaml, the security guarantees of OCaml are not violated by memory vulnerabilities in the C code.</p>
<p>For performance reasons, the compartmentalisation and the fat pointer scheme for C are accelerated with hardware support. Importantly, since OCaml has language-level safety, FIDES does not use fat pointers for OCaml and pays neither the memory overhead of fat pointers nor the performance overhead of checking their validity at runtime. Given that MirageOS lets the developers build much of the typical operating system services directly in OCaml, FIDES is able to pay the additional performance cost for security only for C code.</p>
<p>Since OCaml and C code can interact seamlessly the developer benefits from both safety and flexibility. The compartmental approach limits additional vulnerabilities and fat-pointers ensure that C code interacts safely with OCaml code. On top of this, FIDES deploys secure-by-design MirageOS applications, extending the MirageOS backend to execute on bare-metal RISC-V processors. In this way, from metal to application, FIDES combines open-source technologies with strong security guarantees to give users a transparent, safe, and flexible solution.</p>
<h3 style="position:relative;"><a href="https://tarides.com/feed.xml#benefits" aria-label="benefits permalink" class="anchor before"><svg aria-hidden="true" focusable="false" height="16" version="1.1" viewbox="0 0 16 16" width="16"><path fill-rule="evenodd" d="M4 9h1v1H4c-1.5 0-3-1.69-3-3.5S2.55 3 4 3h4c1.45 0 3 1.69 3 3.5 0 1.41-.91 2.72-2 3.25V8.59c.58-.45 1-1.27 1-2.09C10 5.22 8.98 4 8 4H4c-.98 0-2 1.22-2 2.5S3 9 4 9zm9-3h-1v1h1c1 0 2 1.22 2 2.5S13.98 12 13 12H9c-.98 0-2-1.22-2-2.5 0-.83.42-1.64 1-2.09V6.25c-1.09.53-2 1.84-2 3.25C6 11.31 7.55 13 9 13h4c1.45 0 3-1.69 3-3.5S14.5 6 13 6z"></path></svg></a>Benefits</h3>
<p>Let's conclude with the key benefits of using FIDES:</p>
<ul>
<li><strong>Secure:</strong> Memory safety is guaranteed, and FIDES seamlessly supports linking to unsafe C code without compromising overall safety.</li>
<li><strong>Light-Weight:</strong> Does not require a memory management unit, an OS, or a hypervisor to guarantee isolation. It protects bare metal resource-constrained systems.</li>
<li><strong>User Friendly:</strong> FIDES is designed to let users stay oblivious to the compartment policies and use unmodified OCaml and C code without issue. The FIDES design also permits a security engineer to perform critical compartment mapping tasks as part of deployment rather than development.</li>
</ul>
<h2 style="position:relative;"><a href="https://tarides.com/feed.xml#until-next-time" aria-label="until next time permalink" class="anchor before"><svg aria-hidden="true" focusable="false" height="16" version="1.1" viewbox="0 0 16 16" width="16"><path fill-rule="evenodd" d="M4 9h1v1H4c-1.5 0-3-1.69-3-3.5S2.55 3 4 3h4c1.45 0 3 1.69 3 3.5 0 1.41-.91 2.72-2 3.25V8.59c.58-.45 1-1.27 1-2.09C10 5.22 8.98 4 8 4H4c-.98 0-2 1.22-2 2.5S3 9 4 9zm9-3h-1v1h1c1 0 2 1.22 2 2.5S13.98 12 13 12H9c-.98 0-2-1.22-2-2.5 0-.83.42-1.64 1-2.09V6.25c-1.09.53-2 1.84-2 3.25C6 11.31 7.55 13 9 13h4c1.45 0 3-1.69 3-3.5S14.5 6 13 6z"></path></svg></a>Until Next Time!</h2>
<p>What sets FIDES apart is its focus on security, combining safety features for software and hardware to provide a secure and convenient way to deploy applications. Running MirageOS on the Shakti RISC-V processor shows excellent potential for delivering a high-performance, secure user experience.</p>
<p>Connect with us on <a href="https://twitter.com/tarides_">X</a> and <a href="https://www.linkedin.com/company/tarides">LinkedIn</a> for the latest updates on our projects. You can also <a href="https://tarides.com/contact/">contact us</a> directly on our website with any feedback or questions. See you next time!</p>